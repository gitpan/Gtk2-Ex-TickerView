#!/usr/bin/perl

# Copyright 2007, 2008 Kevin Ryde

# This file is part of Gtk2-Ex-TickerView.
#
# Gtk2-Ex-TickerView is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 3, or (at your option) any later
# version.
#
# Gtk2-Ex-TickerView is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License along
# with Gtk2-Ex-TickerView.  If not, see <http://www.gnu.org/licenses/>.


use strict;
use warnings;
use Gtk2 '-init';

use Glib::Ex::ConnectProperties;
use Gtk2::Ex::TickerView;

sub exception_handler {
  my ($msg) = @_;
  print __FILE__,": ", $msg;
  eval { require Devel::StackTrace; };
  if (! $@) {
    my $trace = Devel::StackTrace->new;
    print $trace->as_string;
  }
  return 1; # stay installed
}
Glib->install_exception_handler (\&exception_handler);


my $toplevel = Gtk2::Window->new('toplevel');
$toplevel->set_default_size (500, -1);
$toplevel->signal_connect (destroy => sub { Gtk2->main_quit; });

my $hbox = Gtk2::HBox->new (0, 0);
$toplevel->add ($hbox);

my $left_vbox = Gtk2::VBox->new (0, 0);
$hbox->pack_start ($left_vbox, 0,0,0);

my $right_vbox = Gtk2::VBox->new (0, 0);
$hbox->pack_start ($right_vbox, 1,1,0);


my $model = Gtk2::ListStore->new ('Glib::String');
foreach my $str ('yy', 'zz-bb', '<b>xx</b>', 'fjdks', '32492', "abc\ndef") {
  my $iter = $model->append;
  $model->set_value ($iter, 0, $str);
}

if (0) {
  {
    my $ticker = Gtk2::Ex::TickerView->new (model => $model,
                                            attributes => {text => 0}
                                           );
    my $menu = $ticker->menu;
    $ticker->signal_connect (destroy => sub {
                               print __FILE__,": ticker destroy signal\n";
                             });
    $menu->signal_connect (destroy => sub {
                             print __FILE__,": menu destroy signal\n";
                           });
  }
  Gtk2->main;
}

Gtk2::Rc->parse_string (<<'HERE');
style "my_style" {
  fg[ACTIVE]   = { 1.0, 1.0, 1.0 }
  text[ACTIVE] = { 1.0, 1.0, 1.0 }
  bg[ACTIVE]   = { 0, 0, 0 }
  base[ACTIVE] = { 0, 0, 0 }
}
widget "*.Gtk2__Ex__TickerView" style "my_style"
HERE

my $ticker = Gtk2::Ex::TickerView->new (model => $model,
                                        frame_rate => 500,
                                        speed => 500,
                                        run => 0);
print __FILE__,": ticker name=", $ticker->get_name || 'undef',
  " initial flags: ", $ticker->flags,"\n";
{ my $color = $ticker->get_style->fg('normal');
  print __FILE__,": ticker fg[NORMAL]: ", $color->to_string, "\n";
}
{ my $color = $ticker->get_style->fg('active');
  print __FILE__,": ticker fg[ACTIVE]: ", $color->to_string, "\n";
}

my $renderer = Gtk2::CellRendererText->new;
# $renderer->set (width=>0);
$ticker->pack_start ($renderer, 1);
$ticker->set_attributes ($renderer, text => 0);
$ticker->set_cell_data_func
  ($renderer, sub {
     my ($ticker, $renderer, $model, $iter) = @_;
     $renderer->set (foreground_gdk => $ticker->get_style->fg($ticker->state),
                     foreground_set => 1);
   });

if (1) {
  my $renderer = Gtk2::CellRendererText->new;
  $ticker->pack_start ($renderer, 1);
  $ticker->set_attributes ($renderer, markup => 0);
}
if (0) {
  my $renderer = Gtk2::CellRendererText->new;
  $renderer->set (text => 'VIS');
  $ticker->pack_start ($renderer, 1);
  $ticker->set_cell_data_func
    ($renderer, sub {
       my ($ticker, $renderer, $model, $iter, $userdata) = @_;
       my $len = length ($model->get ($iter, 0));
       $renderer->set('visible', $len != 5);
     });
}
$ticker->signal_connect (destroy => sub {
                           print __FILE__,": ticker destroy signal\n";
                         });
$right_vbox->pack_start ($ticker, 0,0,0);

if (0) {
  my $menu = $ticker->menu;
  $menu->signal_connect (destroy => sub {
                           print __FILE__,": menu destroy signal\n";
                         });
  require Gtk2::Ex::CheckMenuItem::Property;
  my $item = Gtk2::Ex::CheckMenuItem::Property->new_with_label ('Foo');
  $item->show;
  $menu->append ($item);
}

{
  my $button = Gtk2::CheckButton->new_with_label ('Run');
  Glib::Ex::ConnectProperties->new ([$ticker,'run'],
                                    [$button,'active']);
  $left_vbox->pack_start ($button, 0, 0, 0);
}
{
  my $button = Gtk2::CheckButton->new_with_label ('Show');
  Glib::Ex::ConnectProperties->new ([$ticker,'visible'],
                                    [$button,'active']);
  $left_vbox->pack_start ($button, 0, 0, 0);
}
{
  my $button = Gtk2::CheckButton->new_with_label ('Sensitive');
  Glib::Ex::ConnectProperties->new ([$ticker,'sensitive'],
                                    [$button,'active']);
  $left_vbox->pack_start ($button, 0, 0, 0);
}
{
  my $button = Gtk2::CheckButton->new_with_label ('Fixed Height');
  Glib::Ex::ConnectProperties->new ([$ticker,'fixed-height-mode'],
                                    [$button,'active']);
  $left_vbox->pack_start ($button, 0, 0, 0);
}
{
  my $button = Gtk2::CheckButton->new_with_label ('Forced Height');
  $button->signal_connect (toggled => sub {
                             $ticker->set (height_request
                                           => $button->get_active
                                           ? 50 : -1);
                           });
  $left_vbox->pack_start ($button, 0, 0, 0);
}
{
  my $button = Gtk2::CheckButton->new_with_label ('Model');
  $button->set_active ($ticker->get('model'));
  $button->signal_connect (toggled => sub {
                             $ticker->set (model => ($button->get_active
                                                     ? $model : undef));
                           });
  $left_vbox->pack_start ($button, 0, 0, 0);
}
{
  my $button = Gtk2::CheckButton->new_with_label ('pointer-motion-hint');
  $button->signal_connect (toggled => sub {
                             my $window = $ticker->window;
                             my $events = $window->get_events;
                             if ($button->get_active) {
                               $events = $events + 'pointer-motion-hint-mask';
                             } else {
                               $events = $events - 'pointer-motion-hint-mask';
                             }
                             $window->set_events ($events);
                             print __FILE__,": events $events\n";
                           });
  $left_vbox->pack_start ($button, 0, 0, 0);

  # As of Gtk 2.12.10 _gtk_tooltip_handle_event() very rudely does a
  # gdk_event_request_motions() even when there's no 'has-tooltip' or
  # anything tooltip set for the relevant widget.  (The call to
  # _gtk_tooltip_handle_event() is hard-coded in gtk_main_do_event().)
  #
  # request_motions() can be avoided by sticking the tooltip into "keyboard
  # mode" with a show-help call.  This makes it possible to see if the
  # is_hint handling in the ticker is doing the right thing, as opposed to
  # the tooltip always doing the server request.
  #
  print __FILE__,":show-help tooltip ",
    $ticker->signal_emit ('show-help', 'tooltip'), "\n";
}
{
  my $button = Gtk2::Button->new_with_label ('Redraw');
  $button->signal_connect (clicked => sub { $ticker->queue_draw; });
  $left_vbox->pack_start ($button, 0, 0, 0);
}
{
  my $direction_model = Gtk2::ListStore->new ('Glib::String',
                                              'Gtk2::TextDirection');
  $direction_model->set ($direction_model->append, 0, 'LtoR', 1, 'ltr');
  $direction_model->set ($direction_model->append, 0, 'RtoL', 1, 'rtl');

  my $hbox = Gtk2::HBox->new;
  $left_vbox->pack_start ($hbox, 0,0,0);
  $hbox->pack_start (Gtk2::Label->new('Direction'), 0,0,0);

  my $combobox = Gtk2::ComboBox->new_with_model ($direction_model);
  my $renderer = Gtk2::CellRendererText->new;
  $combobox->pack_start ($renderer, 1);
  $combobox->set_attributes ($renderer, 'text', 0);
  $combobox->set_active (0);
  $combobox->signal_connect (changed => sub {
                               my $iter = $combobox->get_active_iter;
                               my $dir = $direction_model->get_value ($iter,1);
                               $ticker->set_direction ($dir);
                             });
  $hbox->pack_start ($combobox, 1,1,0);

  $ticker->signal_connect
    (direction_changed => sub {
       print __FILE__,": ticker direction changed\n";
       my $idx = ($ticker->get_direction eq 'ltr' ? 0 : 1);
       $combobox->set_active ($idx);
     });
}
{
  my $state_model = Gtk2::ListStore->new ('Glib::String');
  my %state_to_index;
  my $i = 0;
  foreach my $elem (Glib::Type->list_values('Gtk2::StateType')) {
    my $state = $elem->{'nick'};
    $state_model->set ($state_model->append, 0 => $state);
    $state_to_index{$state} = $i;
    $i++;
  }
  my $hbox = Gtk2::HBox->new;
  $left_vbox->pack_start ($hbox, 0,0,0);
  $hbox->pack_start (Gtk2::Label->new('State'), 0,0,0);

  my $combobox = Gtk2::ComboBox->new_with_model ($state_model);
  my $renderer = Gtk2::CellRendererText->new;
  $combobox->pack_start ($renderer, 1);
  $combobox->set_attributes ($renderer, 'text', 0);
  $combobox->set_active (0);
  $hbox->pack_start ($combobox, 1,1,0);
  $combobox->signal_connect (changed => sub {
                               my $iter = $combobox->get_active_iter;
                               my $state = $state_model->get_value ($iter,0);
                               print __FILE__,": set state $state\n";
                               $ticker->set_state ($state);
                             });

  $ticker->signal_connect
    (state_changed => sub {
       my $state = $ticker->state;
       print __FILE__,": ticker state changed to $state\n";
       $combobox->set_active ($state_to_index{$state});
     });
}
{
  my $button = Gtk2::Button->new_with_label ('Goto Start');
  $button->signal_connect (clicked => sub { $ticker->scroll_to_start; });
  $left_vbox->pack_start ($button, 0, 0, 0);
}
{
  my $button = Gtk2::Button->new_with_label ('Scroll');
  $button->signal_connect (clicked => sub { $ticker->scroll_pixels (10); });
  $left_vbox->pack_start ($button, 0, 0, 0);
}
{
  my $button = Gtk2::Button->new_with_label ('Scroll Back');
  $button->signal_connect (clicked => sub { $ticker->scroll_pixels (-10); });
  $left_vbox->pack_start ($button, 0, 0, 0);
}
{
  my $button = Gtk2::Button->new_with_label ('Unparent');
  $button->signal_connect (clicked => sub { $right_vbox->remove ($ticker); });
  $left_vbox->pack_start ($button, 0, 0, 0);
}
{
  my $button = Gtk2::Button->new_with_label ('Destroy');
  $button->signal_connect (clicked => sub { $ticker->destroy; });
  $left_vbox->pack_start ($button, 0, 0, 0);
}
{
  my $button = Gtk2::CheckButton->new_with_label ('DebugUps');
  $button->set_tooltip_markup ("Set Gtk2::Gdk::Window->set_debug_updates to flash invalidated regions");
  $button->set_active (0);
  $button->signal_connect (toggled => sub {
                             Gtk2::Gdk::Window->set_debug_updates
                                 ($button->get_active);
                           });
  $left_vbox->pack_start ($button, 0, 0, 0);
}
{
  my $button = Gtk2::Button->new_with_label ('Reorder Reverse');
  $button->signal_connect (clicked => sub {
                             my $rows = $model->iter_n_children (undef);
                             $model->reorder (reverse 0 .. $rows-1);
                           });
  $left_vbox->pack_start ($button, 0, 0, 0);
}
{
  my $button = Gtk2::Button->new_with_label ('Delete First');
  $button->signal_connect (clicked => sub {
                             $model->remove ($model->get_iter_first);
                           });
  $left_vbox->pack_start ($button, 0, 0, 0);
}
{
  my $button = Gtk2::Button->new_with_label ('Delete Last');
  $button->signal_connect (clicked => sub {
                             my $rows = $model->iter_n_children (undef);
                             $model->remove ($model->get_iter_from_string
                                             ($rows-1));
                           });
  $left_vbox->pack_start ($button, 0, 0, 0);
}
my $insert_count = 1;
{
  my $button = Gtk2::Button->new_with_label ('Insert First');
  $button->signal_connect (clicked => sub {
                             $model->insert_with_values (0, 0,
                                                         "x$insert_count");
                             $insert_count++;
                           });
  $left_vbox->pack_start ($button, 0, 0, 0);
}
{
  my $button = Gtk2::Button->new_with_label ('Insert Last');
  $button->signal_connect (clicked => sub {
                             my $rows = $model->iter_n_children (undef);
                             $model->insert_with_values ($rows, 0,
                                                         "x$insert_count\nx\nx\nx\nx\nx\nx\nx\nx\nx\nx\nx\nx\nx\nx\nx\nx\nx\nx\nx\nx\nx\nx\nx\n");
                             $insert_count++;
                           });
  $left_vbox->pack_start ($button, 0, 0, 0);
}
{
  my $button = Gtk2::Button->new_with_label ('Renderer Add');
  $button->signal_connect (clicked => sub {
                             $renderer = Gtk2::CellRendererText->new;
                             $renderer->set (text => 'Foo');
                             $ticker->pack_start ($renderer, 0);
                           });
  $left_vbox->pack_start ($button, 0, 0, 0);
}
{
  my $button = Gtk2::Button->new_with_label ('Renderers Clear');
  $button->signal_connect (clicked => sub {
                             print __FILE__,": ticker renderers clear\n";
                             $ticker->clear;
                           });
  $left_vbox->pack_start ($button, 0, 0, 0);
}
{
  my $pspec = $ticker->find_property ('frame-rate');
  my $adj = Gtk2::Adjustment->new (0,
                                   $pspec->get_minimum,
                                   $pspec->get_maximum,
                                   1, 10, 10);
  my $hbox = Gtk2::HBox->new;
  $left_vbox->pack_start ($hbox, 0,0,0);
  $hbox->pack_start (Gtk2::Label->new('frame-rate'), 0,0,0);
  my $spin = Gtk2::SpinButton->new ($adj, 1, 0);
  $hbox->pack_start ($spin, 1,1,0);
  Glib::Ex::ConnectProperties->new ([$ticker,'frame-rate'],
                                    [$spin,'value']);
}
{
  my $pspec = $ticker->find_property ('speed');
  my $adj = Gtk2::Adjustment->new (0,
                                   $pspec->get_minimum,
                                   $pspec->get_maximum,
                                   1, 10, 10);
  my $hbox = Gtk2::HBox->new;
  $left_vbox->pack_start ($hbox, 0,0,0);
  $hbox->pack_start (Gtk2::Label->new('speed'), 0,0,0);
  my $spin = Gtk2::SpinButton->new ($adj, 1, 0);
  $hbox->pack_start ($spin, 1,1,0);
  Glib::Ex::ConnectProperties->new ([$ticker,'speed'],
                                    [$spin,'value']);
}
{
  my $button = Gtk2::Button->new_with_label ('Quit');
  $button->signal_connect (clicked => sub { $toplevel->destroy; });
  $left_vbox->pack_start ($button, 0, 0, 0);
}



{
  my $treeview = Gtk2::TreeView->new_with_model ($model);
  $treeview->set (reorderable => 1);
  $right_vbox->pack_start ($treeview, 1,1,0);

  my $column = Gtk2::TreeViewColumn->new_with_attributes
    ("Item", $renderer, text => 0);
  $column->set (resizable => 1);
  $treeview->append_column ($column);
}

$toplevel->show_all;
Gtk2->main;
exit 0;
