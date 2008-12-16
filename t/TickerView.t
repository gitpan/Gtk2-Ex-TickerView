#!/usr/bin/perl

# Copyright 2007, 2008 Kevin Ryde

# This file is part of Gtk2-Ex-TickerView.
#
# Gtk2-Ex-TickerView is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as published
# by the Free Software Foundation; either version 3, or (at your option) any
# later version.
#
# Gtk2-Ex-TickerView is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
# Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with Gtk2-Ex-TickerView.  If not, see <http://www.gnu.org/licenses/>.

use strict;
use warnings;
use Gtk2::Ex::TickerView;
use Test::More tests => 54;

# return true if there's any signal handlers connected to $obj
sub any_signal_connections {
  my ($obj) = @_;
  my @connected = grep {$obj->signal_handler_is_connected ($_)} (0 .. 500);
  if (@connected) {
    diag "$obj signal handlers connected: ",join(' ',@connected),"\n";
    return 1;
  }
  return 0;
}

ok ($Gtk2::Ex::TickerView::VERSION >= 11);
ok (Gtk2::Ex::TickerView->VERSION  >= 11);

require Gtk2;
diag ("Perl-Gtk2 version ",Gtk2->VERSION);
diag ("Perl-Glib version ",Glib->VERSION);
diag ("Compiled against Glib version ",
      Glib::MAJOR_VERSION(), ".",
      Glib::MINOR_VERSION(), ".",
      Glib::MICRO_VERSION());
diag ("Running on       Glib version ",
      Glib::major_version(), ".",
      Glib::minor_version(), ".",
      Glib::micro_version());
diag ("Compiled against Gtk version ",
      Gtk2::MAJOR_VERSION(), ".",
      Gtk2::MINOR_VERSION(), ".",
      Gtk2::MICRO_VERSION());
diag ("Running on       Gtk version ",
      Gtk2::major_version(), ".",
      Gtk2::minor_version(), ".",
      Gtk2::micro_version());

#------------------------------------------------------------------------------
# _hash_keys_remap

{
  my %h = (1 => 100, 2 => 200);
  Gtk2::Ex::TickerView::_hash_keys_remap (\%h, sub { $_[0] });
  is_deeply ([ @h{1,2} ], [ 100,200 ],
             '_hash_keys_remap 2 unchanged');

  Gtk2::Ex::TickerView::_hash_keys_remap (\%h, sub { $_[0]==1?2:1 });
  is_deeply ([ @h{1,2} ], [ 200,100 ],
             '_hash_keys_remap 2 swap');
}

{
  my %h = (0 => 100, 1 => 110, 2 => 120, 3 => 130);
  Gtk2::Ex::TickerView::_hash_keys_remap (\%h, sub { ($_[0]+1)%4 });
  is_deeply ([ @h{0,1,2,3} ], [ 130,100,110,120 ],
             '_hash_keys_remap 4 rotate');
}


#------------------------------------------------------------------------------
# _normalize

{
  my $model = Gtk2::ListStore->new ('Glib::Int');
  $model->set ($model->insert(0), 0=>100);
  $model->set ($model->insert(1), 0=>110);
  $model->set ($model->insert(2), 0=>120);
  my $ticker = Gtk2::Ex::TickerView->new (model => $model);

  my ($x, $index) = Gtk2::Ex::TickerView::_normalize ($ticker, 0, 3);
  is ($x, 0, '_normalize() ok when index past end');
  is ($index, 3, '_normalize() ok when index past end');
}
{
  my $model = Gtk2::ListStore->new ('Glib::Int');
  $model->set ($model->insert(0), 0=>100);
  $model->set ($model->insert(1), 0=>110);
  $model->set ($model->insert(2), 0=>120);
  my $ticker = Gtk2::Ex::TickerView->new (model => $model);

  my ($x, $index) = Gtk2::Ex::TickerView::_normalize ($ticker, -1, 3);
  is ($x, undef, '_normalize() adjust x when index past end');
  is ($index, 3, '_normalize() adjust x when index past end');
}


#------------------------------------------------------------------------------
# _do_rows_reordered

{
  my $model = Gtk2::ListStore->new ('Glib::Int');
  $model->set ($model->insert(0), 0=>100);
  $model->set ($model->insert(1), 0=>110);
  $model->set ($model->insert(2), 0=>120);
  $model->set ($model->insert(3), 0=>130);
  my $ticker = Gtk2::Ex::TickerView->new (model => $model);
  my $row_widths = $ticker->{'row_widths'} = {0=>100, 1=>110, 2=>120};

  # array[newpos] = oldpos, ie. where the row used to be
  $model->reorder (1,2,3,0);
  is_deeply ([ map {$model->get($model->iter_nth_child(undef,$_),0)} 0..3 ],
             [ 110,120,130,100 ],
             'reordered elements');

  is ($ticker->{'want_index'}, 3,
      'reordered want_index');
  is_deeply ([ @{$row_widths}{0,1,2,3} ],
             [ 110,120,undef,100 ],
             'reordered widths');
}


#------------------------------------------------------------------------------
{
  my $all_zeros = Gtk2::Ex::TickerView::_make_all_zeros_proc();
  ok (! &$all_zeros(0,0));
  ok (  &$all_zeros(0,0));
}
{
  my $all_zeros = Gtk2::Ex::TickerView::_make_all_zeros_proc();
  ok (! &$all_zeros(0,0));
  ok (! &$all_zeros(1,0));
  ok (! &$all_zeros(2,0));
  ok (  &$all_zeros(0,0));
}
{
  my $all_zeros = Gtk2::Ex::TickerView::_make_all_zeros_proc();
  ok (! &$all_zeros(1,0));
  ok (! &$all_zeros(2,0));
  ok (! &$all_zeros(0,0));
  ok (! &$all_zeros(1,0));
  ok (! &$all_zeros(2,0));
  ok (  &$all_zeros(0,0));
}
{
  my $all_zeros = Gtk2::Ex::TickerView::_make_all_zeros_proc();
  ok (! &$all_zeros(0,1));
  ok (! &$all_zeros(0,0));
  ok (! &$all_zeros(0,0));
}
{
  my $all_zeros = Gtk2::Ex::TickerView::_make_all_zeros_proc();
  ok (! &$all_zeros(0,0));
  ok (! &$all_zeros(1,0));
  ok (! &$all_zeros(0,1));
}

{
  my $ticker = Gtk2::Ex::TickerView->new;
  isa_ok ($ticker, 'Gtk2::Ex::TickerView', 'ticker');
  require Gtk2;
  my $r1 = Gtk2::CellRendererText->new;
  my $r2 = Gtk2::CellRendererText->new;
  $ticker->pack_start ($r1, 0);
  $ticker->pack_start ($r2, 0);

  $ticker->reorder ($r1, 0);
  is_deeply ([$ticker->GET_CELLS], [$r1, $r2],
             'reorder 2 no change');

  $ticker->reorder ($r1, 1);
  is_deeply ([$ticker->GET_CELLS], [$r2, $r1],
             'reorder 2 swap');

  $ticker->reorder ($r1, 0);
  is_deeply ([$ticker->GET_CELLS], [$r1, $r2],
             'reorder 2 swap back');
}

{
  my $ticker = Gtk2::Ex::TickerView->new;
  my $r1 = Gtk2::CellRendererText->new;
  my $r2 = Gtk2::CellRendererText->new;
  my $r3 = Gtk2::CellRendererText->new;
  $ticker->pack_start ($r1, 0);
  $ticker->pack_start ($r2, 0);
  $ticker->pack_start ($r3, 0);
  
  $ticker->reorder ($r1, 0);
  is_deeply ([$ticker->GET_CELLS], [$r1, $r2, $r3],
             'reorder 3 no change');
  
  $ticker->reorder ($r1, 1);
  is_deeply ([$ticker->GET_CELLS], [$r2, $r1, $r3],
             'reorder 3 swap first two');
  
  $ticker->reorder ($r3, 0);
  is_deeply ([$ticker->GET_CELLS], [$r3, $r2, $r1],
             'reorder 3 last to first');
  
  $ticker->reorder ($r3, 2);
  is_deeply ([$ticker->GET_CELLS], [$r2, $r1, $r3],
             'reorder 3 first back to last');
}

{
  my $m1 = Gtk2::ListStore->new ('Glib::String');
  my $m2 = Gtk2::ListStore->new ('Glib::String');
  my $ticker = Gtk2::Ex::TickerView->new (model => $m1);
  require Scalar::Util;
  Scalar::Util::weaken ($m1);
  $ticker->set(model => $m2);
  is ($m1, undef, "shouldn't keep a reference to previous model");
}

{
  my $ticker = Gtk2::Ex::TickerView->new;
  Scalar::Util::weaken ($ticker);
  is ($ticker, undef, 'garbage collected when weakened - empty');
}
{
  my $store = Gtk2::ListStore->new ('Glib::String');
  my $ticker = Gtk2::Ex::TickerView->new (model => $store);
  Scalar::Util::weaken ($ticker);
  is ($ticker, undef, 'garbage collected when weakened - with model');
}
{
  my $store = Gtk2::ListStore->new ('Glib::String');
  my $ticker = Gtk2::Ex::TickerView->new (model => $store);
  $ticker->set (model => undef);
  my $get_model = $ticker->get('model');
  is ($get_model, undef, 'unset model from ticker');
}
{
  my $store = Gtk2::ListStore->new ('Glib::String');
  my $ticker = Gtk2::Ex::TickerView->new (model => $store);
  $ticker->set (model => undef);
  ok (! any_signal_connections ($store),
      'no signal handlers left on model when unset');
}

SKIP: {
  if (! Gtk2->init_check) { skip 'due to no DISPLAY available', 5; }

  {
    my $ticker = Gtk2::Ex::TickerView->new;
    my $path = $ticker->get_path_at_pos (0, 0);
    is ($path, undef, "get_path_at_pos when no model");

    my $store = Gtk2::ListStore->new ('Glib::String');
    $ticker->set (model => $store);
    $path = $ticker->get_path_at_pos (0, 0);
    is ($path, undef, "get_path_at_pos when empty model, and unrealized");

    $ticker->set (model => undef);
    $store->insert_with_values (0, 0=>'foo');
    $ticker->set (model => $store);
    $path = $ticker->get_path_at_pos (0, 0);
    isa_ok ($path, 'Gtk2::TreePath');
    if ($path) { $path = $path->to_string; }
    is ($path, '0', "get_path_at_pos when non-empty and unrealized");

    $ticker->set (model => undef);
    $store->remove ($store->iter_nth_child(undef,0));
    $ticker->set (model => $store);
    my $toplevel = Gtk2::Window->new ('toplevel');
    $toplevel->add ($ticker);
    $toplevel->show_all;
    $path = $ticker->get_path_at_pos (0, 0);
    if ($path) { $path = $path->to_string; }
    is ($path, undef, "get_path_at_pos when empty and realized");
  }
}

SKIP: {
  if (! Gtk2::Ex::TickerView->isa('Gtk2::Buildable')) {
    skip 'due to no Gtk2::Buildable interface', 6;
  }

  my $builder = Gtk2::Builder->new;
  $builder->add_from_string (<<'HERE');
<interface>
  <object class="Gtk2__Ex__TickerView" id="ticker">
    <property name="width-request">200</property>
    <child>
      <object class="GtkCellRendererText" id="renderer">
        <property name="xpad">10</property>
      </object>
      <attributes>
        <attribute name="text">0</attribute>
      </attributes>
    </child>
  </object>
  <object class="GtkCellView" id="cellview">
    <property name="width-request">200</property>
    <child>
      <object class="GtkCellRendererText" id="ren2">
        <property name="xpad">10</property>
      </object>
      <attributes>
        <attribute name="text">0</attribute>
      </attributes>
    </child>
  </object>
</interface>
HERE

  my $ticker = $builder->get_object('ticker');
  isa_ok ($ticker, 'Gtk2::Ex::TickerView', 'ticker from buildable');

  my $renderer = $builder->get_object('renderer');
  isa_ok ($renderer, 'Gtk2::CellRendererText', 'renderer from buildable');
  is_deeply ([ $ticker->GET_CELLS ], [ $renderer ],
             'GET_CELLS ticker from buildable');

  my $store = Gtk2::ListStore->new ('Glib::String');
  $ticker->set (model => $store);
  my $iter = $store->insert_with_values (0, 0=>'foo');
  $ticker->_set_cell_data ($iter);  # from Gtk2::Ex::CellLayout::Base
  is ($renderer->get ('text'), 'foo',
      'renderer from buildable attribute set');

  # Something fishy seen in gtk 2.12.1 (with gtk2-perl 1.180, 1.183 or
  # 1.200) that $builder stays non-undef when weakened, though the objects
  # within it weaken away as expected.  Some of the ref counting changed
  # from what the very first gtkbuilder versions did, so think it's a gtk
  # problem already fixed, so just ignore that test.
  #
  Scalar::Util::weaken ($builder);
  Scalar::Util::weaken ($ticker);
  Scalar::Util::weaken ($renderer);
  # is ($builder,  undef, 'builder weakened');
  is ($ticker,   undef, 'ticker from builder weakened');
  is ($renderer, undef, 'renderer from builder weakened');
}

exit 0;
