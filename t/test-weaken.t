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
use Test::More;

my $have_test_weaken = eval "use Test::Weaken 1.002; 1";
if (! $have_test_weaken) {
  plan skip_all => "due to Test::Weaken 1.002 not available -- $@";
}

plan tests => 5;

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

sub wait_for_event {
  my ($widget, $signame) = @_;
  my $done = 0;
  my $sig_id = $widget->signal_connect ($signame => sub {
                                          $done = 1;
                                          return 0; # Gtk2::EVENT_PROPAGATE
                                        });
  my $timer_id = Glib::Timeout->add (30_000, # 30 seconds
                                     sub {
                                       print "Oops, timeout on $signame";
                                       exit 1;
                                     });
  $widget->get_display->sync;

  my $count = 0;
  while (! $done) {
    Gtk2->main_iteration;
    $count++;
  }
  diag "wait_for_event('$signame'): ran $count events/iterations\n";

  $widget->signal_handler_disconnect ($sig_id);
  Glib::Source->remove ($timer_id);
}


#-----------------------------------------------------------------------------

{
  my @weaken = Test::Weaken::poof(sub { Gtk2::Ex::TickerView->new });
  diag "Test-Weaken ", explain \@weaken;
  my $unfreed = @{$weaken[2]} + @{$weaken[3]};
  is ($unfreed, 0, 'Test::Weaken deep garbage collection');
}

is (Test::Weaken::poof(sub {
                         my $store = Gtk2::ListStore->new ('Glib::String');
                         my $ticker = Gtk2::Ex::TickerView->new
                           (model => $store);
                         [ $ticker, $store ]
                       }),
    0, 'Test::Weaken with a model set');

is (Test::Weaken::poof(sub {
                         my $s1 = Gtk2::ListStore->new ('Glib::String');
                         my $ticker = Gtk2::Ex::TickerView->new
                           (model => $s1);
                         my $s2 = Gtk2::ListStore->new ('Glib::String');
                         $ticker->set (model => $s2);
                         [ $ticker, $s1, $s2 ]
                       }),
    0, 'Test::Weaken a model set then changed to another model');

#------------------------------------------------------------------------------
# timer run and stop

my $have_display = Gtk2->init_check;
diag "have_display: ",($have_display ? "yes" : "no");

SKIP: {
  $have_display or skip 'due to no DISPLAY available', 2;

  my $timer_running;
  my @weaken = Test::Weaken::poof
    (sub {
       my $store = Gtk2::ListStore->new ('Glib::String');
       $store->set ($store->append, 0 => 'foo');
       my $ticker = Gtk2::Ex::TickerView->new (model => $store,
                                               width_request => 100,
                                               height_request => 100);
       my $renderer = Gtk2::CellRendererText->new;
       $renderer->set (text => 'hello'); # dummy
       $ticker->pack_start ($renderer, 0);

       my $toplevel = Gtk2::Window->new ('toplevel');
       $toplevel->add ($ticker);
       $toplevel->show_all;
       wait_for_event ($ticker, 'map_event');

       $timer_running = defined $ticker->{'timer'};
       return [ $toplevel, $ticker, $renderer, $store ];
     },
     sub {
       my ($aref) = @_;
       my $toplevel = $aref->[0];
       $toplevel->destroy;
     });
  ok ($timer_running, 'toplevel shown - timer runs');

  diag "Test-Weaken ", explain \@weaken;
  my $unfreed = @{$weaken[2]} + @{$weaken[3]};
  is ($unfreed, 0, 'Test::Weaken deep garbage collection -- running timer');
}

exit 0;
