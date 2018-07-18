# SPDX-License-Identifier: LGPL-2.1-or-later
# Copyright © 2008-2018 ANSSI. All Rights Reserved.
package CLIP::Pkg::Install;

use 5.008008;
use strict;
use warnings;
use File::Basename;
use File::Path;
use File::Copy;
use CLIP::Pkg::Base ':all';
use CLIP::Logger ':all';

require Exporter;

our @ISA = qw(Exporter);

our %EXPORT_TAGS = ( 'all' => [ qw(
clippkg_check_installed
clippkg_check_removed
clippkg_install_sanity_check
clippkg_get_tbx_pkgs
clippkg_autoremove
clippkg_remove_optionals
clippkg_install_error_recovery
clippkg_upgrade
clippkg_check_upgrade
) ] );

our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

our @EXPORT = qw(
	
);

our $VERSION = '1.1.15';

=head1 NAME

CLIP::Pkg::Install - Perl extension managing CLIP packages installation.

=head1 VERSION

Version 1.1.15

=head1 SYNOPSIS

  use CLIP::Pkg::Install;
  use CLIP::Pkg::Install ':all';

=head1 DESCRIPTION

CLIP::Pkg::Install provides a set of helper functions for package installation
from a local mirror on CLIP systems. This includes the management of a
local cache, where packages to be installed have their signatures and fields 
checked, and are then stored so that the package installation script can re-install
an older package in case of error.

CLIP::Pkg::Install uses the basic helpers from CLIP::Pkg::Install for, among 
others, package parsing and checking, and the CLIP::Logger module to log its
outputs.

=head1 EXPORT

No functions or variables are exported by default. The module defines a single
Exporter tag, ':all', which exports the following functions:

=over 4

=item * 

B<clippkg_check_installed()>

=item * 

B<clippkg_check_removed()>

=item *

B<clippkg_install_sanity_check()>

=item *

B<clippkg_autoremove()>

=item *

B<clippkg_remove_optionals($)>

=item * 

B<clippkg_get_tbx_pkgs()>

=item * 

B<clippkg_install_error_recovery()>

=item * 

B<clippkg_upgrade()>

=item * 

B<clippkg_check_upgrade()>

=back

=cut

###############################################################
#                          VARIABLES                          #
###############################################################

=head1 VARIABLES

CLIP::Pkg::Install can be configured through the following variables:

=cut

=over 4
                       ####################
		       #      Paths       #
		       ####################


=item B<$CLIP::Pkg::Install::g_apt_cache_dir>

Full path to the current install cache. This should be the parent 
directory of the C<archives> directory where packages to be installed
will be copied or linked, e.g. C</var/cache/apt>.

=cut

our $g_apt_cache_dir;


=item B<$CLIP::Pkg::Install::g_dpkg_install_dir>

Install root for dpkg commands, i.e. the [path] passed as 
C<dpkg --root [path] [cmd]>. Defaults to an empty string, which
is the same as C</>.

=cut

our $g_dpkg_install_dir = "";

=item B<$CLIP::Pkg::Install::g_arch>

Current dpkg architecture, e.g. C<i386>. Defaults to empty.

=cut

our $g_arch = "";
                       ########################
		       #      Constants       #
		       ########################

# Temporary lists created in cache dir
# for error recovery.
# Each list contains one package per line

 # New packages possibly copied into the cache
 # Each line is <pname>_<pver>_<arch>.deb
my $g_pending_list = "pending";
 # New packages possibly installed (and not upgraded)
 # Each line is <pname>_<new pver>
my $g_tbi_list = "installing";
 # Packages possibly upgraded (old versions)
 # Each line is <pname>_<old_pver>
my $g_tbu_list = "upgrading";
 # Packages possibly removed 
 # Each line is <pname>_<pver>
my $g_tbr_list = "removing";

# Shortcut
my $g_apt_opts = $CLIP::Pkg::Base::g_apt_opts;

=back

In addition to those module-specific variables, CLIP::Pkg::Install is also 
indirectly affected by the configuration variables supported by the 
CLIP::Pkg::Base and CLIP::Logger modules, which it includes, and in particular
by the CLIP::Pkg::Base package and configuration check variables.

=cut

###############################################################
#                          FUNCTIONS                          #
###############################################################

=head1 FUNCTIONS

CLIP::Pkg::Install defines the following functions:

=cut

                       #################################
		       #      Local status tests       #
		       #################################

=head2 Tests on local status

=over 4

=item B<clippkg_check_installed($pname, $ver)>

Checks that a package named $pname is locally installed.
If $ver is defined, checks that the installed package's version
is $ver.
Returns 1 if the package (with optionnal version) is installed,
0 otherwise.

=cut

sub clippkg_check_installed($$) {
	my ($pname, $pver) = @_;

	my $fields;
	unless (defined ($fields = clippkg_get_installed_fields($pname, 'Version', 0))) {
		return 0;
	} 
	return 1 unless (defined ($pver));

	if ($pver eq $fields->{'Version'}) {
		return 1;
	} else {
		return 0;
	}
}

=item B<clippkg_check_removed($pname)>

Checks that no package named $pname is installed on the system.
Returns 1 if no such package is installed, 0 otherwise.

=cut

sub clippkg_check_removed($) {
	my $pname = shift;

	open PIPE, "dpkg-query --admindir=$g_dpkg_admin_dir -s \'$pname\' 2>&1|";
	my @output = <PIPE>; 
	close PIPE;
	if ($?) {
		foreach (@output) {
			/package '$pname' is not installed/ and return 1;
		}
		# Something weird happened...
		clip_warn "dpkg-query error on $pname";
		foreach (@output) {
			clip_warn "dpkg-query output: $_";
		}
		return 0;
	} else {
		foreach (@output) {
			/^Status: .* ok.*not-installed/ and return 1;
			/^Status: .* deinstall.*ok/ and return 1;
		}
		return 0;
	}
}
	
                       #################################
		       #      Upgrade simulation       #
		       #################################

=back

=head2 Upgrade simulation

=over 4

=item I<CLIP::Pkg::Install::parse_simulation($sim, $tbi, $tbr, $tbu)>

Internal use only. Given an apt-get (cmd) -s output in $sim (list reference, one
entry per output line), builds three hashes, respectively referenced by $tbi, $tbr and $tbu, 
one each for packages to be installed (including upgrades), packages to be removed and packages to be
upgraded, matching package names to versions. Each hash is only built if the corresponding
hash reference argument is defined.

Returns 1 if the whole simulation has been parsed OK, 0 otherwise.

=cut

sub parse_simulation($$$$) {
	my ($sim,$tbi,$tbr,$tbu) = @_;

	my $pname;
	my $pver;
	foreach my $line (@{$sim}) {
		if (defined ($tbi) and $line =~ /^Inst (\S+) (?:\[[^\]]+\] )?\((\S+)/) {
			$pname = $1;
			$pver = $2;

			if (defined ($tbi->{$pname})) {
				clip_warn "duplicate install entry for $pname: "
						."$pver / $tbi->{$pname}";
				return 0;
			}
			$tbi->{$pname} = $pver;

			if (defined ($tbu) and $line =~ /^Inst \S+ \[([^\]]+)\]/) {
				if (defined ($tbu->{$pname})) {
					clip_warn "duplicate upgrade entry for $pname :"
							."$pver / $tbu->{$pname}";
					return 0;
				}
				$pver = $1;
				$tbu->{$pname} = $pver;
			}

			next;
		} 
		if (defined ($tbr) and $line =~ /^Remv (\S+) \[([^\]]+)\]/) {
			$pname = $1;
			$pver = $2;

			if (defined ($tbr->{$pname})) {
				clip_warn "duplicate removal entry for $pname: "
							."$pver / $tbr->{$pname}";
				return 0;
			}
			$tbr->{$pname} = $pver;

			next;
		}
	}

	return 1;
}

=item B<clippkg_install_sanity_check($list)> 

Check that a list of package names to be removed does not contain any configuration.
Check that all packages to be removed have the expected priority.

Returns 1 if the list is ok for removal (no configuration will be removed), 0 otherwise.

=cut

sub clippkg_install_sanity_check($) {
	my $list = shift;

	foreach my $pname (@{$list}) {
		if (clippkg_is_conf($pname)) {
			clip_warn "sanity check : operation would remove configuration $pname, aborting";
			return 0;
		}
	}
	if (defined($CLIP::Pkg::Base::g_pkg_opts->{"Priority"})) {
		my $hash;
		foreach my $pname (@{$list}) {
			unless (defined($hash = clippkg_get_installed_fields($pname, "Priority", 0))) {
				clip_warn "could not get priority of $pname, which is to be removed";
				return 0;
			}

			if (lc($hash->{"Priority"}) ne lc($CLIP::Pkg::Base::g_pkg_opts->{"Priority"})) {
				clip_warn "wrong priority on package $pname, which is to be removed";
				return 0;
			}
		}
	}

	return 1;
}

=item B<clippkg_get_tbx_pkgs($clist, $tbi, $tbr, $tbu, $retry)>

Generates hashes of pkgs to be installed and removed, based on a list $clist 
(space-separated string ) of configurations to be upgraded.

This function runs C<apt-get install -s> with $clist as argument, to list 
all dependencies to be installed / removed. The output of that command is 
parsed to generate up to three hashes, based on which of the $tbi, $tbr and
$tbu references are defined. The hashes generated for each of these references
are respectively :

=over 8

=item $tbi

Hash keyed by package names, of packages to be installed (both new installs
and upgrades). The hash values are the versions of the packages to be installed 
('new' versions for upgrades).

=item $tbr

Hash keyed by package names, of packages to be removed without replacement
from the system. The hash values are the currently installed versions of the 
packages.

=item $tbu

Hash keyed by package names, of packages to be upgraded. The hash values are
the currenly installed versions of the packages, i.e. the 'old' versions.

=back

The function will try $retry times (if non-null) to fix errors by calling
clippkg_apt_error().

The function returns 1 on success, and 0 on failure.

=cut

sub clippkg_get_tbx_pkgs($$$$$) {
	my ($clist, $tbi, $tbr, $tbu, $retry) = @_;

	# Needed for reinstall 
	$CLIP::Pkg::Base::g_dpkg_reinst_cache = $g_apt_cache_dir if ($retry);
RETRY:
	open PIPE, "echo \"Yes, do as I say!\" | apt-get install -f $g_apt_opts -s $clist 2>&1 |";
	my @output = <PIPE>;
	close PIPE;
	if ($?) {
		clip_warn "failed to simulate installation of [$clist]";
		foreach (@output) {
			clip_warn "apt output: $_";
		}
		return 0 unless ($retry);
		return 0 unless clippkg_apt_error(\@output);
		$retry--;
		goto RETRY;
	}
	
	unless (parse_simulation(\@output, $tbi, $tbr, $tbu)) {
		clip_warn "failed to parse install simulation for $clist";
	}

CHECKLOOP:
	foreach my $conf (keys %{$tbi}) {
		next CHECKLOOP unless (clippkg_is_conf($conf));
		next CHECKLOOP if ($clist =~ /\b$conf\b/);
		clip_warn "update would install a non candidate configuration!";
		clip_warn "probably due to optional packages pulling a configuration that was masked because of ConfDeps...";
		return 0;
	}
	clip_log "install candidates: ".(join " ", keys %{$tbi}) 
					if (defined ($tbi) and keys %{$tbi});
	clip_log "removal candidates: ".(join " ", keys %{$tbr}) 
					if (defined ($tbr) and keys %{$tbr});
	clip_log "upgrade candidates: ".(join " ", keys %{$tbu}) 
					if (defined ($tbu) and keys %{$tbu});
	return 1;
}

                       ##############################
		       #      Install helpers       #
		       ##############################

=back

=head2 Install helpers

=over 4

=item I<CLIP::Pkg::Install:remove_conf($cname, $tbi)>

Remove a configuration named $cname, and all of its to-be-installed dependencies,
from the local cache. A hash matching to-be-installed packages and 
configurations names to versions is passed as $tbi, and updated by 
deleting the keys corresponding to removed packages.

Returns 1 on success, 0 on failure.

=cut 

sub remove_conf($$) {
	my ($cname, $tbi) = @_;

	my $cfull = "$cname"."_".$tbi->{$cname}."_"."$g_arch.deb";
	my $cpath = "$g_apt_cache_dir/archives/$cfull";

	my $deps;
	unless (defined($deps = clippkg_get_full_depends($cpath, 2))) {
		clip_warn "failed to list dependencies for configuration $cfull, "
				."cannot remove it from cache";
		return 0;
	}

	my @tbr = ();
PKGLOOP:
	foreach my $pkg (@{$deps}) {
		next PKGLOOP unless (-f "$g_apt_cache_dir/archives/$pkg");
		if ($pkg =~ /^([^_]+)_([^_]+)/) {
			my $pname = $1;
			my $pver = $2;
			my $cver;
			next PKGLOOP unless (defined($cver = $tbi->{$pname}));
			next PKGLOOP unless ($cver eq $pver);

			clip_log "removing $pkg from the install cache, as it was pulled "
					."by $cfull";

			push @tbr, ($pname);
			unless (unlink "$g_apt_cache_dir/archives/$pkg") {
				clip_warn "failed to remove dependency $pkg from cache";
				return 0;
			}
		} else {
			clip_warn "invalid dependency atom in $cfull: $pkg";
			next PKGLOOP;
		}
	}
	unless (unlink $cpath) {
		clip_warn "failed to remove configuration $cfull from cache";
		return 0;
	}
	
	# Update tbi to avoid useless checks afterwards
	delete $tbi->{$cname};
	delete @{$tbi}{@tbr};

	return 1;
}
		
=item I<CLIP::Pkg::Install::check_remove_confs($clist, $tbi)>

Check all new configurations in the install cache, listed by $clist (which is
a reference to a list of short names), and remove from it those that do not 
pass their signature or fields checks, along with all their dependencies.

A hash matching to-be-installed packages and configurations names to versions 
is passed as $tbi, and updated by deleting the keys corresponding to removed packages.

Returns 1 on success, 0 on failure.

=cut

sub check_remove_confs($$) {
	my ($list, $tbi) = @_;

	my $len = $#{$list};
	my $idx = 0; 

	while ($idx < $len + 1) {
		my $cname = ${$list}[$idx];

		my $cfull = "$cname"."_".$tbi->{$cname}."_"."$g_arch.deb";
		my $cpath = "$g_apt_cache_dir/archives/$cfull";

		unless (clippkg_check_pkg($cpath, 1)) {
			clip_warn "removing configuration $cfull from the "
					."install cache";
			return 0 unless (remove_conf($cname, $tbi));
			# Remove conf from list to remove its allowed 
			# optional packages
			splice @{$list}, $idx, 1;
			$len--;
		} else {
			$idx++;
		}
	}

	return 1;
}

=item I<CLIP::Pkg::Install::check_remove_pkgs($plist, $clist, $tbi)>

Check all new packages (other than configurations) in the install cache, 
as listed by $plist (which is a reference to a list of short names), and 
remove from it those that do not pass their signature or fields checks, and those
which are not allowed given the configurations currently installed or pending 
installation (the latter of which are listed in $clist).

A hash matching to-be-installed packages and configurations names to versions 
is passed as $tbi, and updated by deleting the keys corresponding to removed packages.

Returns 1 on success, 0 on failure.

=cut

sub check_remove_pkgs($$$) {
	my ($plist, $clist, $tbi) = @_;

	my @cfull_list = map { $_."_".$tbi->{$_}."_".$g_arch.".deb" } (@{$clist});

	my $allowed;
	# Create list of allowed packages from installed confs, overriden by valid configurations
	# currently in the install cache.
	return 0 
		unless (defined($allowed = clippkg_list_allowed_optional(\@cfull_list, "$g_apt_cache_dir/archives")));
	
	my $count = 0;
	my $total = $#{$plist} + 1;
PKGLOOP:
	foreach my $pname (@{$plist}) {
		$count++;
		next PKGLOOP unless (defined($tbi->{$pname}));
		my $pfull = "$pname"."_".$tbi->{$pname}."_"."$g_arch.deb";
		my $ppath = "$g_apt_cache_dir/archives/$pfull";

		next PKGLOOP unless (-f $ppath);

		next PKGLOOP 
			if (clippkg_check_optional($pfull, $allowed) 
				and clippkg_check_pkg_msg($ppath, 0, "[$count/$total]"));
		
		unless (unlink $ppath) {
			clip_warn "failed to remove package $pfull from cache";
			return 0;
		}
		delete $tbi->{$pname};
	}

	return 1;
}


=item I<CLIP::Pkg::Install::update_cache_install($clist, $tbi)>

Internal use only.
Downloads packages to be installed from the local mirror into the install cache,
then checks all those new packages (signatures, priority, impact, etc.). In this
call, $clist is a string containing a space-separated list of configurations 
to be upgraded, while $tbi is a reference to a hash matching package names to 
versions, for all packages to be installed (as returned by clippkg_get_tbx_pkgs()).

The new packages are downloaded into the cache through an C<apt-get install -d $clist>,
but then signatures and fields checks are performed on all packages referenced in $tbi,
thus providing a consistency check between $clist and $tbi.

Packages that fail their signature check or fields check are removed from the install
cache, but do not cause the function to return an error. This removal may cause
some or all configuration upgrades to be impossible because of failed dependencies,
but this is dealt with when performing the actual upgrade.

Returns 1 on success, 0 on failure.

=cut

sub update_cache_install($$) {
	# confs to be updated, pkgs + confs to be updated (dl
	my ($clist, $tbi) = @_;

	# Move needed packages in the cache
	open PIPE, "echo \"Yes, do as I say!\" | apt-get install -f --ignore-missing $g_apt_opts -d $clist 2>&1 |";
	my @output = <PIPE>;
	close PIPE;
	if ($?) {
		clip_warn "failed to get dependencies of [$clist] into the cache";
		foreach (@output) {
			clip_warn "apt output: $_";
		}
		return 0;
	}
	
	my @tbi_confs = ();
	my @tbi_pkgs = ();
	# Check packages, remove those that don't match from the cache,
	# and list them in @removed to be removed from %tbi afterwards
	my @rm_keys = ();
	foreach my $pname (keys %{$tbi}) {
		if (clippkg_is_conf($pname)) {
			push @tbi_confs, ($pname);
		} else {
			push @tbi_pkgs, ($pname);
		}
	}

	return 0 unless (check_remove_confs(\@tbi_confs, $tbi));
	return 0 unless (check_remove_pkgs(\@tbi_pkgs, \@tbi_confs, $tbi));

	return 1;
}


=item I<CLIP::Pkg::Install::do_installs($clist)>

Internal use only.  Installs as many packages as possible, among the list passed as $clist, 
from what's available in the install cache. Some of the upgrades previously planned may 
not be performed at this point, because signature or fields checks by update_cache_install() 
may result in some of the packages being removed from the install cache, which in turn may 
result in some missing dependencies for planned configuration upgrades. It is thus necessary to 
later check for what was actually upgraded in this run, for example by calling
list_install().

Returns 1 on success, 0 on error.

=cut

sub do_installs($) {
	my $list = shift;

	clip_log "Installing packages. This might take a while - relaxen und watchen das blinkenlichten... ";

	open PIPE, "echo \"Yes, do as I say!\" | apt-get install $list -f $g_apt_opts --no-download --ignore-missing 2>&1 |";
	my @output = <PIPE>;
	close PIPE;
	if ($?) {
		clip_warn "failed to install packages";
		foreach (@output) {
			clip_warn "apt output: $_";
		}
		return 0;
	} else {
		return 1;
	}
}

=item I<CLIP::Pkg::Install::list_install($tbi, $tbr, $tbu)>

Checks packages that were effectively installed / removed by a do_installs()
call, among those that were planned to be installed / removed. The planned
installs, removals and upgrades are passed as the $tbi, $tbr and $tbu 
references (which must be defined), as returned by a clippkg_get_tbx_pkgs()
call.

The actual installs and removals are not returned to the caller, but simply
logged out through clip_log() calls.

Returns 0 if nothing was installed (even if some packages were removed, which
is highly unlikely without a configuration install), and 1 if some packages 
were installed.

=cut

sub list_install($$$) {
	my ($tbi, $tbr, $tbu) = @_;
	my $ret = 0;

	foreach my $pname (keys %{$tbi}) {
		my $pver = $tbi->{$pname};
		if (clippkg_check_installed($pname, $pver)) {
			if (defined($tbu->{$pname})) {
				clip_log "$pname [$pver] has been upgraded";
			} else {
				clip_log "$pname [$pver] has been installed (new package)";
			}
			$ret = 1;
		} else {
			clip_warn "$pname [$pver] was not installed or upgraded";
		}
	}

	foreach my $pname (keys %{$tbr}) {
		my $pver = $tbr->{$pname};
		if (clippkg_check_removed($pname)) {
			clip_log "$pname [$pver] was removed";
		} else {
			clip_warn "$pname [$pver] was not removed";
		}
	}

	return $ret;
}

                       ###########################
		       #      Package Removal    #
		       ###########################

=back

=head2 Package removal functions

=over 4

=item B<CLIP::Pkg::Install::list_autoremove()>

Internal use only. List packages to be autoremoved. Returns a reference to a list
(possibly empty) of short package names, for those packages are to be removed, 
or C<undef> in case of error.

=cut

sub list_autoremove() {

	my %tbr = (); # Packages to be removed
	my %tbi = (); # Packages to be installed (for sanity checking only)
	
	open PIPE, "echo \"Yes, do as I say!\" | apt-get autoremove $g_apt_opts -s 2>&1 |";
	my @output = <PIPE>;
	close PIPE;
	if ($?) {
		clip_warn "failed to simulate removal of unneeded packages";
		foreach (@output) {
			clip_warn "apt output: $_";
		}
		return undef;
	}
	
	unless (parse_simulation(\@output, \%tbi, \%tbr, undef)) {
		clip_warn "failed to parse autoremove simulation";
		return undef;
	}

	if (keys %tbi) {
		clip_warn "auto-removal would install packages. Something is very wrong...";
		return undef;
	}

	my @list = ();
	# Filter-out wrong priority auto-removals, until I can understand why it happens
	if (defined($CLIP::Pkg::Base::g_pkg_opts->{"Priority"})) {
		my $hash;
	PRIOLOOP:
		foreach my $pname (keys %tbr) {
			unless (defined($hash = clippkg_get_installed_fields($pname, "Priority", 0))) {
				clip_warn "could not get priority of $pname, which is to be removed";
				next PRIOLOOP;
			}

			if (lc($hash->{"Priority"}) ne lc($CLIP::Pkg::Base::g_pkg_opts->{"Priority"})) {
				clip_warn "wrong priority on package $pname, which is to be removed";
				next PRIOLOOP;
			}
			push @list, ($pname);
		}
	}

	return undef unless (clippkg_install_sanity_check(\@list));

	return \@list;
}

=item B<CLIP::Pkg::Install::list_optional_remove($dist)>

Internal use only. Returns a reference to a list of short package names, for those 
optional packages that used to be selected for install, but are no longer so, and
are still installed on the system.
Returns C<undef> in case of error.

=cut

sub list_optional_remove($) {
	my $dist = shift;
	my $curfiles = $CLIP::Pkg::Base::g_optional_pkg_files;
	return [] unless (@{$curfiles});

	my @lastfiles = map { "$g_apt_cache_dir"."/".(basename $_) } @{$curfiles};
	
	my $curpkgs;
	my $lastpkgs;
	return undef
		unless (defined($curpkgs = clippkg_list_optional($curfiles)));
	return undef
		unless (defined($lastpkgs = clippkg_list_optional(\@lastfiles)));

	my $fields;
	unless (defined($fields = clippkg_get_installed_fields(clippkg_get_confname($dist, 'apps', undef), 'Depends', 1))) {
		clip_warn "No dependencies for the $dist apps!";
		return undef;
	}
	my @fdeps = split ", ", $fields->{"Depends"};
	my @deps = map { s/ \(.*\)$//; $_ } @fdeps;
	my @tbr_pkgs = ();

PKGLOOP:
	foreach my $pkg (@{$lastpkgs}) {
		next PKGLOOP if (grep { /\b$pkg\b/ } @{$curpkgs});

		unless (clippkg_check_installed($pkg, undef)) {
			clip_warn "optional package file copy is out of date";
			next PKGLOOP;
		}
		if (grep {/^$pkg$/} @deps) {
			clip_log "package $pkg is not optional anymore";
			next PKGLOOP;
		}

		clip_log "optional package $pkg should no longer be installed";
		push @tbr_pkgs, ($pkg);
	}

	return undef unless (clippkg_install_sanity_check(\@tbr_pkgs));
	return \@tbr_pkgs;
}

=item B<CLIP::Pkg::Install::check_optional_remove($list)>

Simulate a removal of optional packages listed in $list, and perform a
sanity check on that simulation, to make sure that no configuration would
be removed.

Returns 1 if the removal is accepted, 0 otherwise.

=cut

sub check_optional_remove($) {
	my $list = shift;
	my $plist = join " ", @{$list};

	open PIPE, "echo \"Yes, do as I say!\" | apt-get remove $g_apt_opts -s $plist 2>&1 |";
	my @output = <PIPE>;
	close PIPE;
	if ($?) {
		clip_warn "failed to simulate removal of optional packages";
		foreach (@output) {
			clip_warn "apt output: $_";
		}
		return 0;
	}
	
	my %tbr = (); # Packages to be removed
	my %tbi = (); # Packages to be installed (for sanity checking only)
	
	unless (parse_simulation(\@output, \%tbi, \%tbr, undef)) {
		clip_warn "failed to parse optional remove simulation";
		return 0;
	}

	if (keys %tbi) {
		clip_warn "optional packages removal would install packages. Something is very wrong...";
		return 0;
	}

	return 0 unless (clippkg_install_sanity_check([keys %tbr]));
	return 1;
}

=item B<CLIP::Pkg::Install::list_removed($list, $errlist)>

Internal use only. Check that a list of packages, referenced by $list, have all
been removed from the system, and output corresponding log messages.

Returns 1 if all packages in $list have been removed, or 0 otherwise.
Packages that have not been removed are pushed on the list referenced by
$errlist, if defined.

=cut

sub list_removed($$) {
	my ($list, $elist) = @_;
	my $ret = 1;

	foreach my $pname (@{$list}) {
		if (clippkg_check_removed($pname)) {
			clip_log "$pname was removed";
		} else {
			clip_warn "$pname was not removed";
			$ret = 0;
			push @{$elist}, ($pname) if (defined($elist));
		}
	}

	return $ret;
}

=item B<clippkg_autoremove()>

Auto-remove packages that are not needed anymore on the system, as long as 
they don't break the sanity checks (no configurations can be removed).

=cut

sub clippkg_autoremove() {
	my $tbr_list;

	unless (defined($tbr_list = list_autoremove())) {
		clip_warn "failed to get list of packages to autoremove";
		return 0;
	}

	my $plist = join " ", @{$tbr_list};

	unless ($plist) {
		clip_log "no packages to autoremove";
		return 1;
	}

	clip_log "packages $plist are no longer needed, will try to remove them";

	open PIPE, "echo \"Yes, do as I say!\" | apt-get remove $g_apt_opts $plist 2>&1 |";
	my @output = <PIPE>;
	close PIPE;
	if ($?) {
		clip_warn "failed to remove packages $plist";
		foreach (@output) {
			clip_warn "apt output: $_";
		}
		return 0;
	}

	# No error here
	clip_warn "not all packages could be removed" 
				unless (list_removed($tbr_list, undef));
	
	return 1;
}

=item B<CLIP::Pkg::Install::write_unremoved($curfile, $lastfile, $elist)>

Internal use only. Update the cached copy of a given optional file after 
optional package removal, when some packages (from this file or another one) 
could not be removed. This first extracts, from the list of packages that could
not be removed (passed as reference $elist), those packages that could not be
removed from the current optional package file (i.e. those that where in the 
previous copy $lastfile, and not in the current one $curfile). It then
copies the current optional package file over its cached copy, then re-adds to
that file those packages that could not be removed from the file.

This is a slow path, which should only be used in case some packages could not
be removed. Otherwise, a simple copy of $curfile over $lastfile would be enough.

Returns 1 on success, 0 on error.

=cut

sub write_unremoved($$$) {
	my ($curfile, $lastfile, $elist) = @_;

	my @leftover = ();
	if (open IN, "<", "$lastfile") {
		my @last = <IN>;
		close IN;
		my @choplast = map { chomp $_; $_ } @last;
		foreach my $pname (@{$elist}) {
			push @leftover, ($pname)
				if (grep {/^$pname$/} @choplast);
		}
	}
	unless (copy($curfile, $lastfile)) {
		clip_warn "failed to copy $curfile to $curfile";
		return 0;
	}
	if (@leftover) {
		unless (open OUT, ">>", $lastfile) {
			clip_warn "failed to open $lastfile for writing";
			return 0;
		}
		foreach my $pname (@leftover) {
			print OUT "$pname\n";
		}
		close OUT;
	}
	return 1;
}

=item B<clippkg_remove_optionals($dist)>

Remove optional packages that are not needed anymore on the system.
This is done by comparing the current optional package list file and
the copy of that file that was created in the install cache directory
on the previous run, and removing those packages that appear in the latter
and not in the current file.

After removing packages, the current optional package list is copied to the 
install cache.

Returns 1 on success, 0 on error.

=cut

sub clippkg_remove_optionals($) {
	my $dist = shift;
	my $tbr_list;

	unless (defined($tbr_list = list_optional_remove($dist))) {
		clip_warn "could not list optional packages to remove";
		return 0;
	}

	unless (check_optional_remove($tbr_list)) {
		clip_warn "optional packages removal did not pass sanity check";
		return 0;
	}

	if ($#{$tbr_list} == -1) {
		clip_log "no optional packages to remove";
		return 1;
	}

	my $plist = join " ", @{$tbr_list};
	clip_log "optional packages $plist are no longer selected, will try to remove them";

	open PIPE, "echo \"Yes, do as I say!\" | apt-get purge $g_apt_opts $plist 2>&1 |";
	my @output = <PIPE>;
	close PIPE;
	if ($?) {
		clip_warn "failed to remove packages $plist";
		foreach (@output) {
			clip_warn "apt output: $_";
		}
		return 0;
	}

	my @elist = ();

	clip_warn "not all packages could be removed"
			unless (list_removed($tbr_list, \@elist));


	# Clean up dependencies, without errors
	clip_warn "failed to remove unneeded packages"
			unless (clippkg_autoremove());

	my $curfiles = $CLIP::Pkg::Base::g_optional_pkg_files;
	return 0 unless (@{$curfiles});

	if (@elist) {
		# slow path : some packages could not be removed
		foreach my $curfile (@{$curfiles}) {
			my $lastfile = "$g_apt_cache_dir"."/".(basename $curfile);
			unless (write_unremoved($curfile, $lastfile, \@elist)) {
				clip_warn "failed to copy (slow path) $curfile to $lastfile";
				return 0;
			}
		}
		return 0;
	} else {
		# fast path
		foreach my $curfile (@{$curfiles}) {
			my $lastfile = "$g_apt_cache_dir"."/".(basename $curfile);
			unless (copy($curfile, $lastfile)) {
				clip_warn "failed to copy $curfile to $lastfile";
				return 0;
			}
		}
		return 1;
	}
}

	

                       ###############################
		       #      Error Management       #
		       ###############################

=back

=head2 Error management helpers

=over 4

=item I<CLIP::Pkg::Install::clean_mirror()>

Internal use only.
Removes pending packages left in the install cache by a previous
interrupted install run, based on the contents of the "pending"
file left in the cache by clippkg_upgrade().
Returns 1 if all pending packages that were actually in the cache
have been removed, 0 otherwise.

=cut

sub clean_mirror() {
	unless (open IN, "<", "$g_apt_cache_dir/$g_pending_list") {
		clip_warn "could not open pending list";
		return 0;
	}
	my @pending = <IN>;
	close IN;

	foreach my $pkg (@pending) {
		chomp $pkg;
		next unless (-f "$g_apt_cache_dir/archives/$pkg");

		if (unlink "$g_apt_cache_dir/archives/$pkg") {
			clip_log "removed pending $pkg";
		} else {
			clip_warn "could not remove pending $pkg";
			return 0;
		}
	}

	unless (unlink "$g_apt_cache_dir/$g_pending_list") {
		clip_warn "could not remove pending list";
		return 0;
	}

	return 1;
}

=item I<CLIP::Pkg::Install::undo_installs()>

Internal use only.
Uninstalls newly installed (not upgraded) packages after an interrupted
install run, based on the contents of the "installing" file left in
the install cache by clippkg_upgrade(). Only those packages in the 
"installing" file that are effectively installed are removed.

Package removal is done with the C<--force-all> dpkg option, to make
sure it is not blocked by dependency or other issues.

Returns 1 if all packages in the "installing" file that were effectively
installed have been removed, 0 otherwise.

=cut

sub undo_installs() {
	unless (open IN, "<", "$g_apt_cache_dir/$g_tbi_list") {
		clip_warn "could not open new installs list";
		return 0;
	}
	my @insts = <IN>;
	close IN;

	my $ret = 1;
	# Try to remove as many packages as possible
	# Do not block in case of error, just log it
	foreach my $pname (@insts) {
		chomp $pname;
		$pname =~ s/_.*$//;
		next unless (clippkg_check_installed($pname, undef));

		my $rootarg = "";
		$rootarg = "--root $g_dpkg_install_dir" if ($g_dpkg_install_dir);
		open PIPE, "dpkg --admindir $g_dpkg_admin_dir $rootarg --force-all -r $pname 2>&1|";
		my @output = <PIPE>;
		close PIPE;
		if ($?) {
			clip_warn "dpkg -r $pname failed";
			foreach (@output) {
				clip_warn "dpkg output: $_";
			}
			$ret = 0;
		} else {
			clip_log "removed newly installed $pname";
		}
	}

	# Remove the file even if we didn't manage to undo every install -
	# there's little chance we will do better next time, and we should
	# not block all future updates...
	unless (unlink "$g_apt_cache_dir/$g_tbi_list") {
		clip_warn "could not remove new installs list";
		return 0;
	}

	return $ret;
}
			
=item I<CLIP::Pkg::Install::_install_from_file($file, $msg)>

Internal use only. Internal helper for the undo_removes() and undo_upgrades()
helpers.
Installs specific versions of packages, as listed in the file $file (one 
per line, full package names). This can be done either to force-downgrade
a recently upgraded package, or to re-install a recently uninstalled package.
The log output of this functions is adapted accordingly by the $msg argument.

Package installation is done with the C<--force-all> dpkg option, to make
sure it is not blocked by dependency or other issues. It is preceded by a 
signature check (but no fields check) on the package file.

For this re-installation / downgrade loop to work, the install cache needs to
keep a copy of every installed package at least until the end of an install run
that upgrades them. This is done automatically by clippkg_upgrade(), which only
prunes the install cache once installation has been successfully performed.

Returns 1 if all packages in the $file file have been installed, 0 otherwise.

=cut

sub _install_from_file($$) {
	my ($list, $msg) = @_;
	unless (open IN, "<", "$g_apt_cache_dir/$list") {
		clip_warn "could not open $list list";
		return 0;
	}
	my @pkgs = <IN>;
	close IN;

	my $ret = 1;
	my $pname;
	my $pver;
	foreach my $pbase (@pkgs) {
		chomp $pbase;
		unless ($pbase =~ /^([^_]+)_([^_]+)/ ) {
			clip_warn "invalid package name/version: $pbase";
			return 0;
		} else {
			$pname = $1;
			$pver = $2;
		}
		next if (clippkg_check_installed($pname, $pver));

		my $ppath = "$g_apt_cache_dir/archives/$pbase"."_"."$g_arch.deb";

		unless (-f $ppath) {
			clip_warn "could not find package to $msg $pbase";
			$ret = 0;
			next;
		}
		unless (clippkg_check_sig($ppath)) {
			clip_warn "Backup $pbase failed its signature check, won't reinstall it";
			$ret = 0;
			next;
		}

		my $rootarg = "";
		$rootarg = "--root $g_dpkg_install_dir" if ($g_dpkg_install_dir);
		open PIPE, "dpkg --admindir $g_dpkg_admin_dir $rootarg --force-all -i $ppath 2>&1|";
		my @output = <PIPE>;
		close PIPE;
		if ($?) {
			clip_warn "failed to $msg $pbase";
			foreach (@output) {
				clip_warn "dpkg output: $_";
			}
			$ret = 0;
		} else {
			clip_log "managed to $msg $pbase";
		}
	}

	# Remove the list even after errors (same logic as for new installs).
	unless (unlink "$g_apt_cache_dir/$list") {
		clip_warn "could not remove $list list";
		return 0;
	}

	return $ret;
}

=item I<CLIP::Pkg::Install::undo_removes()>

Internal use only.
Re-installs all packages that were uninstalled by an interrupted install run,
based on the contents of the "removing" file left in the install cache by that
script (in particular by clippkg_upgrade(). Signature checks are performed on 
the original packages before re-installing them.
Returns 1 in case of success, 0 on error.

=cut

sub undo_removes() {
	return _install_from_file($g_tbr_list, "reinstall");
}

=item I<CLIP::Pkg::Install::undo_upgrades()>

Internal use only.
Downgrades all packages that were upgraded by an interrupted install run to their
original versions (those that were installed before the interrupted run),
based on the contents of the "upgrading" file left in the install cache by that
script (in particular by clippkg_upgrade(). Signature checks are performed on 
the original packages before re-installing them.
Returns 1 in case of success, 0 on error.

=cut

sub undo_upgrades() {
	return _install_from_file($g_tbu_list, "downgrade to");
}
	
=item B<clippkg_install_error_recovery()>

Recovers from an interrupted (by e.g. a reboot, or an error) install run, by 
(in that order) :

=over 8

=item 1.

Removing all pending packages in the install cache.

=item 2.

Re-installing all packages uninstalled in the previous run.

=item 3.

Downgrading all packages uninstalled in the previous run to their previous version.

=item 4.

Uninstalling all packages that were newly installed in the previous run.

=back

All this is done based on the contents of the "pending", "removing", "upgrading" and 
"installing" files left in the install cache by e.g. clippkg_upgrade().

Returns 1 if all errors were recovered, 0 otherwise.

=cut

sub clippkg_install_error_recovery() {
	clip_debug "checking for errors and leftover bits...";

	if (-e "$g_apt_cache_dir/$g_pending_list") {
		clip_log "removing pending packages from local cache";
		unless (clean_mirror()) {
			clip_warn "failed to clean up local cache, aborting recovery";
			# Recovery / installation must no go on with a 'dirty' cache,
			# as this might allow the installation of packages without sig checks.
			return 0;
		}
	}

	# Should we install, downgrade or remove first ?
	# I believe we should install first, then downgrade, then remove, to 
	# avoid being left with e.g. no dpkg on the system (i.e. we uninstalled
	# dpkg-super, which replaced dpkg, before reinstalling dpkg).
	# This however might lead to problems if the reinstalled packages overwrite
	# files from the packages to be removed...
	if (-e "$g_apt_cache_dir/$g_tbr_list") {
		clip_log "reinstalling removed packages";
		undo_removes();
	}
	if (-e "$g_apt_cache_dir/$g_tbu_list") {
		clip_log "downgrading upgraded packages";
		undo_upgrades();
	}
	if (-e "$g_apt_cache_dir/$g_tbi_list") {
		clip_log "uninstalling new packages";
		undo_installs();
	}
	
	return 1;
}

=item I<CLIP::Pkg::Install::write_list($file, $list)>

Internal use only.
Writes a list of packages referenced by $list in file $file, one 
per line. $file is created as needed.

This is used to create the "pending" error recovery file.

Returns 1 on success, 0 on failure.

=cut

sub write_list($$) {
	my ($file, $listref) = @_;

	unless (open OUT, ">", "$file") {
		clip_warn "could not open $file for writing";
		return 0;
	}
	foreach (@{$listref}) {
		print OUT "$_\n";
	}
	close OUT;
	return 1;
}

=item I<CLIP::Pkg::Install::write_hash($file, $hash, $exclude)>

Internal use only.
Writes to a file $file a list (one per line) of packages referenced 
based on hash $hash, by using keys as package names and values as package 
versions. $file is created as needed.

If $excluded is defined, it is taken as a hash referenced, such that 
packages in $hash are only written out to $file only if their name is not
a valid key in $excluded. 

This is used to create the "installing", "removing" and "upgrading" error
recovery files.

Returns 1 on success, 0 on failure.

=cut

sub write_hash($$$) {
	my ($file, $hash, $exclude) = @_;

	unless (open OUT, ">", "$file") {
		clip_warn "could not open $file for writing";
		return 0;
	}
	foreach my $key (keys %{$hash}) {
		next if (defined($exclude) and defined ($exclude->{$key}));

		print OUT "$key"."_"."$hash->{$key}\n";
	}
	close OUT;
	return 1;
}

sub write_pending($$$) {
	my ($tbi, $tbr, $tbu) = @_;

	my @tbc = map { $_."_".$tbi->{$_}."_".$g_arch.".deb" } (keys %{$tbi});

	unless (write_list("$g_apt_cache_dir/$g_pending_list", \@tbc)) {
		clip_warn "could not write $g_pending_list list, aborting";
		return 0;
	}

	unless (write_hash("$g_apt_cache_dir/$g_tbi_list", $tbi, $tbu)) {
		clip_warn "could not write $g_tbi_list list, aborting";
		goto ERR1;
	}
	unless (write_hash("$g_apt_cache_dir/$g_tbr_list", $tbr, undef)) {
		clip_warn "could not write $g_tbr_list list, aborting";
		goto ERR2;
	}
	unless (write_hash("$g_apt_cache_dir/$g_tbu_list", $tbu, undef)) {
		clip_warn "could not write $g_tbu_list list, aborting";
		goto ERR3;
	}
	
	return 1;

ERR3:
	unlink "$g_apt_cache_dir/$g_tbr_list"
		or clip_warn "failed to remove $g_apt_cache_dir/$g_tbr_list";
ERR2: 
	unlink "$g_apt_cache_dir/$g_tbi_list"
		or clip_warn "failed to remove $g_apt_cache_dir/$g_tbi_list";
ERR1:
	unlink "$g_apt_cache_dir/$g_pending_list"
		or clip_warn "failed to remove $g_apt_cache_dir/$g_pending_list";

	return 0;
}

sub unwrite_pending() {
	foreach my $file ($g_pending_list, $g_tbi_list, $g_tbu_list, $g_tbr_list) {
		unlink "$g_apt_cache_dir/$file" 
			or clip_warn "could not remove $g_apt_cache_dir/$file";
	}
}
                       ###########################
		       #      Full upgrade       #
		       ###########################

=back

=head2 Full upgrade functions

=over 4

=item I<CLIP::Pkg::Install::do_upgrade($clist)>

Internal use only. Helper for clippkg_upgrade(), which does all the work,
except list the names of configurations and optional packages to be upgraded.
This allows clippkg_upgrade() to try doing the same job with configurations only
if the upgrade of configurations and optional packages failed.

Returns 0 on error, 1 if nothing was upgraded, 2 if an upgrade was performed

=cut

sub do_upgrade($) {
	my $clist = shift;

	my %tbi; # packages to be installed (including those to be upgraded)
	my %tbr; # packages to be removed
	my %tbu; # packages to be upgraded 
	my $tbc; # packages to be (possibly) copied into the local cache

	unless (clippkg_get_tbx_pkgs($clist, \%tbi, \%tbr, \%tbu, 2)) {
		return 0;
	}

	unless (keys %tbi) {
		clip_log "nothing to update";
		return 1;
	}
	
	return 0 unless (clippkg_install_sanity_check([keys %tbr]));

	return 0 unless (write_pending(\%tbi, \%tbr, \%tbu));
	
	unless (update_cache_install($clist, \%tbi)) {
		clip_warn "could not update installation cache, aborting";
		goto ERR;
	}

	unless (do_installs($clist)) {
		clip_warn "package installation failed, aborting";
		goto ERR;
	}

	my $ret = list_install(\%tbi, \%tbr, \%tbu);

	unwrite_pending();

	clip_warn "failed to clean the local cache"
		unless (clippkg_prune("$g_apt_cache_dir/archives")); 

	clip_warn "failed to autoremove packages" 
				unless (clippkg_autoremove());

	my $curfiles = $CLIP::Pkg::Base::g_optional_pkg_files;
	foreach my $curfile (@{$curfiles}) {
		my $lastfile = "$g_apt_cache_dir"."/".(basename $curfile);
		unless (copy($curfile, $lastfile)) {
			clip_warn "failed to copy $curfile to $curfile";
		}
	}

	return ($ret + 1);

ERR:
	clippkg_install_error_recovery();
	return 0;
}

=item B<clippkg_upgrade($base, $dist)>

Full upgrade run - installs packages for the local distribution $dist and current
impact settings, from the $dist mirror located at C<$base/mirrors/$dist>. This 
is done using the following steps: 

=over 8

=item 1. 

The install apt index is updated against the local mirror.

=item 2. 

Configuration and optional packages upgrade candidates (with correct distribution 
and impact) are listed by reading the apt index.

=item 3.

The full list of packages to be upgraded or installed is created by simulating the 
upgrade of the list established at step 2.

=item 4.

Those packages are copied or linked into the install cache, and checked there,
with check failures resulting in removal from the cache.

=item 5.

All upgrades possible with the packages remaining in the install cache are performed.

=item 6.

The upgrades actually performed are listed and output to CLIP::Logger.

=item 7.

The install cache is pruned by removing all packages that are not needed any more
(i.e. those that do not belong to an installed configuration).

=back

If steps 3 to 7 fail, they are run again after excluding optional packages from the list
established at step 2, in case upgrading both configurations and optional packages at the
same time proves impossible (e.g. because of unsolvable dependencies, or insufficient
free space on disk).

All along these steps, error recovery capabilities are maintained by creating, at the 
appropriate steps, the "pending", "installing", "upgrading" and "removing" list files, 
for use by clippkg_install_error_recovery(). The function calls the error recovery 
handler itself before returning on an error.

Returns 0 on error, 1 if nothing was upgraded, 2 if an upgrade was performed

=cut

sub clippkg_upgrade($$) {
	my ($mirror_base, $dist) = @_;

	my $mirrorpath = "$mirror_base/mirrors/$dist";

	my $clist;

	unless (clippkg_update_db(2)) {
		clip_warn "could not update apt database, aborting";
		return 0;
	}
	unless (defined($clist = clippkg_list_upgrade_candidates(1))) {
		clip_warn "could not get list of packages to upgrade, aborting";
		return 0;
	}
	unless ($clist) {
		clip_log "nothing to upgrade";
		return 1;
	}

	my $ret = do_upgrade($clist);
	return $ret if ($ret);
	
	clip_warn "full upgrade failed, trying again without optional packages";

	unless (defined($clist = clippkg_list_upgrade_configurations(1))) {
		clip_warn "could not get list of configurations to upgrade, aborting";
		return 0;
	}
	unless ($clist) {
		clip_log "no configurations to upgrade";
		return 1;
	}

	return do_upgrade($clist);
}

=item I<CLIP::Pkg::Install::do_check_upgrade($clist, $mirrorpath, $stop_nomatch, $check)>

Internal use only. Helper for clippkg_check_upgrade(), which does all the work,
except list the names of configurations and optional packages to be upgraded.
This allows clippkg_check_upgrade() to try doing the same job with configurations only
if the upgrade of configurations and optional packages failed.

Returns 0 on error, 1 if no upgrade is available, 2 if an upgrade is available

=cut

sub do_check_upgrade($$$$)
{
	my ($clist, $mirrorpath, $stop_nomatch, $check) = @_;

	my %tbi; # packages to be installed (including those to be upgraded)
	my %tbr; # packages to be removed

	unless (clippkg_get_tbx_pkgs($clist, \%tbi, \%tbr, undef, 0)) {
		return 0;
	}

	unless (keys %tbi) {
		clip_log "nothing to update";
		return 1;
	}
	
	return 0 unless (clippkg_install_sanity_check([keys %tbr]));

	my $count = 0;
	my $total = keys %tbi;
	my $fail_count = 0;
	foreach my $pname (keys %tbi) {
		$count++;

		my $ppath = $pname."_".$tbi{$pname}."_"."$g_arch.deb";
		$ppath = "$mirrorpath/$ppath";

		if ($check) {
			next if (clippkg_check_pkg_msg($ppath, 0, "[$count/$total]"));
		} else {
			next if (-e $ppath);
		}
		
		if ($stop_nomatch) {
			clip_warn "upgrade invalidated because one of its packages "
					."failed its checks";
			return 1;
		}
		# package needs to be removed
		clip_warn "removing $pname from upgrade candidates";
		$fail_count++;
	}

	if ($fail_count == $count) {
		clip_warn "none of the upgrade candidates passed their checks, "
					."invalidating upgrade";
		return 1;
	}

	# Yay, we have an upgrade candidate !
	return 2;
}

=item B<clippkg_check_upgrade($base, $dist, $stop_nomatch, $check)>

Checks for an available upgrade for the current distribution $dist and the current
impact settings, from the local $dist mirror at C<$base/mirrors/$dist>, and 
optionnaly checks all new packages for that upgrade, in the local mirror 
(without touching the local cache).

Full checks (signature, fields, but not Release-Date) are performed on all 
packages to upgrade if $check is non-null. Otherwise, the presence of those
packages in the local mirror is the only check made on upgrade packages.

Just like clippkg_upgrade(), the check is first performed with both available 
configurations and all selected optional packages. If this fails, it is then 
tried with only the configurations, and not the optional packages. This allows
us to deal with e.g. cases where an (old) selected optional package is conflicting
with the main configuration upgrade.

Note: if $stop_nomatch is non-null, no upgrade will be considered available
as soon as any of the packages to be upgraded fails its checks. If that argument
is null, on the other hand, non-matching packages will be simply removed from the
list of packages to be upgraded / installed, and an upgrade will still be considered
available if any packages remain in that list at the end of the checks.

The first approach will reject any upgrades that would fail if tried, but might 
prevent a correct upgrade, if several configugations might be upgraded at the same time
and only one of them fails its checks.

The second approach will never miss a possible upgrade, but might consider 
possible some upgrades that will fail if tried for real.

Returns 0 on error, 1 if no upgrade is available, 2 if an upgrade is available

=cut

sub clippkg_check_upgrade($$$$) {
	my ($mirror_base, $dist, $stop_nomatch, $check) = @_;

	my $mirrorpath = "$mirror_base/mirrors/$dist";

	my $clist;

	unless (clippkg_update_db(0)) {
		clip_warn "could not update apt database, aborting";
		return 0;
	}
	unless (defined($clist = clippkg_list_upgrade_candidates(1))) {
		clip_warn "could not get list of configurations to upgrade, aborting";
		return 0;
	}
	unless ($clist) {
		clip_log "no configurations to update";
		return 1;
	}

	my $ret = do_check_upgrade($clist, $mirrorpath, $stop_nomatch, $check);
	return $ret if ($ret);
	
	clip_warn "full upgrade check failed, trying again without optional packages";

	unless (defined($clist = clippkg_list_upgrade_configurations(1))) {
		clip_warn "could not get list of configurations to upgrade, aborting";
		return 0;
	}
	unless ($clist) {
		clip_log "no configurations to upgrade";
		return 1;
	}

	return do_check_upgrade($clist, $mirrorpath, $stop_nomatch, $check);
}
		
1;
__END__

=back

=head1 SEE ALSO

CLIP::Pkg::Base(3), CLIP::Pkg::Download(3), CLIP::Logger(3), dpkg(1), apt-get(8)

=head1 AUTHOR

Vincent Strubel, E<lt>clip@ssi.gouv.frE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2008-2009 SGDN/DCSSI
Copyright (C) 2010-2012 SGDSN/ANSSI

All rights reserved.

cut
