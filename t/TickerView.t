#!/usr/bin/perl

# Copyright 2007, 2008, 2009 Kevin Ryde

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
use Test::More tests => 45;

my $want_version = 12;
ok ($Gtk2::Ex::TickerView::VERSION >= $want_version, 'VERSION variable');
ok (Gtk2::Ex::TickerView->VERSION  >= $want_version, 'VERSION class method');
Gtk2::Ex::TickerView->VERSION ($want_version);
{
  my $ticker = Gtk2::Ex::TickerView->new;
  ok ($ticker->VERSION >= $want_version, 'VERSION object method');
  $ticker->VERSION ($want_version);
}

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

## no critic (ProtectPrivateSubs)


#------------------------------------------------------------------------------
# _gettime()

{
  my $t1 = Gtk2::Ex::TickerView::_gettime();
  sleep (1);
  my $t2 = Gtk2::Ex::TickerView::_gettime();
  ok ($t2 > $t1, '_gettime() advances');
}


#------------------------------------------------------------------------------
# _hash_keys_remap()

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
# _do_rows_reordered()

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
             'reorder() permutes model rows');

  is ($ticker->{'want_index'}, 3,
      'reorder() permutes want_index');
  is_deeply ([ @{$row_widths}{0,1,2,3} ],
             [ 110,120,undef,100 ],
             'reorder() permutes row_widths');
}


#------------------------------------------------------------------------------
# _make_all_zeros_proc()

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


#------------------------------------------------------------------------------
# reorder() of renderers

{
  my $ticker = Gtk2::Ex::TickerView->new;
  isa_ok ($ticker, 'Gtk2::Ex::TickerView', 'ticker');

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

#------------------------------------------------------------------------------
# weakening

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


exit 0;
