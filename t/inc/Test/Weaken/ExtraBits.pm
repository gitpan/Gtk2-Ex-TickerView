# Test::Weaken::ExtraBits -- some helpers for Test::Weaken

# Copyright 2008, 2009 Kevin Ryde

# Test::Weaken::ExtraBits is shared by several distributions.
#
# Test::Weaken::ExtraBits is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as published
# by the Free Software Foundation; either version 3, or (at your option) any
# later version.
#
# Test::Weaken::ExtraBits is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
# Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this file.  If not, see <http://www.gnu.org/licenses/>.


package Test::Weaken::ExtraBits;
use 5.006;
use strict;
use warnings;

use base 'Exporter';
our @EXPORT_OK = qw(ignore_Class_Singleton
                    ignore_DBI_dr
                    findrefs);
our %EXPORT_TAGS = (all => \@EXPORT_OK);

use constant DEBUG => 0;


sub ignore_Class_Singleton {
  my ($ref) = @_;
  my $class;
  require Scalar::Util;
  return (($class = Scalar::Util::blessed($ref))
          && $ref->isa('Class::Singleton')
          && $class->has_instance
          && $class->instance == $ref);
}

sub ignore_DBI_globals {
  my ($ref) = @_;
  require Scalar::Util;

  if (Scalar::Util::blessed($ref)
      && $ref->isa('DBI::dr')) {
    if (DEBUG) { Test::More::diag ("ignore DBI::dr object -- $ref\n"); }
    return 1;
  }

  return 0;
}

# Return true if $ref is a coderef to function $funcname.
# $funcname is a fully-qualified name like 'Foo::Bar::somefunc'.
# If $funcname doesn't exist then the return is always false.
sub ignore_function {
  my ($ref, $funcname) = @_;
  return (ref $ref eq 'CODE'
          && defined &$funcname
          && $ref == \&$funcname);
}

# Return true if $ref is a coderef to any of the functions in $module.
# $module is a string like 'Foo::Bar'.
sub ignore_module_functions {
  my ($ref, $module) = @_;
  return unless ref $ref eq 'CODE';
  no strict 'refs';
  foreach my $name (keys %{"${module}::"}) {
    my $fullname = "${module}::$name";
    if (defined &$fullname && $ref == \&$fullname) {
      return 1;
    }
  }
  return 0;
}

1;
__END__

=head1 NAME

Test::Weaken::ExtraBits -- various helpers for Test::Weaken

=head1 SYNOPSIS

 use Test::Weaken::ExtraBits;

=head1 EXPORTS

Nothing is exported by default, but the functions can be requested
individually or with C<:all> in the usual way (see L<Exporter>).

    use Test::Weaken::Gtk2 qw(ignore_Class_Singleton);

=head1 FUNCTIONS

=head2 Ignores

=over 4

=item C<< bool = Test::Weaken::Gtk2::ignore_Class_Singleton ($ref) >>

Return true if C<$ref> is the singleton instance of a class using
C<Class::Singleton>.

The current implementation of this function requires C<Class::Singleton>
version 1.04 for its C<has_instance> method.

=item C<< bool = Test::Weaken::Gtk2::ignore_DBI_globals ($ref) >>

Return true if C<$ref> is one of the various C<DBI> module global objects.

Currently this means any C<DBI::dr> driver object, one each of which is
created permanently for each driver loaded, and which C<DBI::db> handles
then refer to.

A bug/misfeature of Perl through to at least 5.10.1 on lvalue C<substr>
means certain scratchpad temporaries of DBI "ImplementorClass" strings end
up held alive after C<DBI::db> and C<DBI::st> objects have finished with
them.  These aren't recognised by C<ignore_DBI_globals> currently.
A workaround is to do a dummy C<DBI::db> creation to flush out the old
scratchpad.

=back

=head1 SEE ALSO

L<Test::Weaken>

=cut
