# Copyright 2008, 2009 Kevin Ryde

# Test::Weaken::Gtk2 is shared by several distributions.
#
# Test::Weaken::Gtk2 is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 3, or (at your option) any later
# version.
#
# Test::Weaken::Gtk2 is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License along
# with this file.  If not, see <http://www.gnu.org/licenses/>.


package Test::Weaken::Gtk2;
use 5.008;
use strict;
use warnings;

use base 'Exporter';
our @EXPORT_OK = qw(contents_container
                    destructor_destroy
                    ignore_default_GdkDisplay);
our %EXPORT_TAGS = (all => \@EXPORT_OK);

use constant DEBUG => 0;


sub contents_container {
  my ($ref) = @_;
  require Scalar::Util;
  return unless Scalar::Util::blessed($ref);
  return unless $ref->isa('Gtk2::Container');

  if (DEBUG) { Test::More::diag ("contents ",ref $ref); }
  return $ref->get_children;
}

sub destructor_destroy {
  my ($ref) = @_;
  if (ref $ref eq 'ARRAY') {
    $ref = $ref->[0];
  }
  $ref->destroy;

  # iterate to make Widget Cursor go unbusy
  require MyTestHelpers;
  MyTestHelpers::main_iterations();
}

# Return true if $ref is the default Gtk2::Display object.
# If Gtk2->init hasn't been called yet then there's no default display
# object and this function returns false.
sub ignore_default_GdkDisplay {
  my ($ref) = @_;
  return unless Gtk2::Gdk::Display->can('get_default'); # if Gtk2 loaded
  return ($ref == (Gtk2::Gdk::Display->get_default || 0));
}

1;
__END__

=head1 NAME

Test::Weaken::Gtk2 -- Gtk2 module helpers for Test::Weaken

=head1 SYNOPSIS

 use Test::Weaken::Gtk2;

=head1 EXPORTS

Nothing is exported by default, but the functions can be requested
individually or with C<:all> in the usual way (see L<Exporter>).

    use Test::Weaken::Gtk2 qw(contents_container);

=head1 FUNCTIONS

=head2 Contents

=over 4

=item C<< @widgets = Test::Weaken::Gtk2::contents_container ($ref) >>

Return the widgets within a C<Gtk2::Container> (or subclass).  If C<$ref> is
not a container widget then the return is an empty list.

=back

=head2 Destructors

=over 4

=item C<< Test::Weaken::Gtk2::destructor_destroy ($ref) >>

If C<$ref> is a C<Gtk2::Object> then call its C<destroy> method.  C<$ref>
can also be an arrayref whose first element is a C<Gtk2::Object>, in which
case that object is acted on.

Generally this is only needed on C<Gtk2::Window> and its subclasses.

=back

=head2 Ignores

=over 4

=item C<< bool = Test::Weaken::Gtk2::ignore_default_GdkDisplay ($ref) >>

Return true if C<$ref> is the default C<Gtk2::Gdk::Display>, per
C<Gtk2::Gdk::Display-E<gt>get_default_display>.

If C<Gtk2> is not yet loaded, or C<Gtk2-E<gt>init> has not been called, then
there's no default display and this function returns false.

=back

=head1 SEE ALSO

L<Test::Weaken>

=cut
