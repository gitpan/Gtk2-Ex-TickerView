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

use ExtUtils::MakeMaker;

WriteMakefile
  (NAME         => 'Gtk2-Ex-TickerView',
   ABSTRACT     => 'Scrolling ticker or newsbar view widget.',
   VERSION_FROM => 'lib/Gtk2/Ex/TickerView.pm',
   AUTHOR       => 'Kevin Ryde <user42@zip.com.au>',
   LICENSE      => 'gpl',
   SIGN         => 1,

   PREREQ_PM    => {
                    # need 1.180 or higher to be able to implement
                    # Gtk2::CellLayout as a GInterface
                    Gtk2 => 1.180,

                    Gtk2::Ex::CellLayout::Base => 0,

                    # actually Gtk2 requires perl 5.8, which has List::Util
                    # and Scalar::Util builtin already
                    List::Util => 0,
                    Scalar::Util => 0,
                   },
   clean        => { FILES => "TickerView.html" },
  );

sub MY::postamble {
  return <<'MAKE_FRAG';

lint:
	perl -MO=Lint lib/Gtk2/Ex/TickerView.pm
	podchecker lib/Gtk2/Ex/TickerView.pm

myman:
	rm -f MANIFEST
	touch SIGNATURE
	make manifest

TickerView.html: lib/Gtk2/Ex/TickerView.pm Makefile
	-perl -MPod::Simple::HTML -e Pod::Simple::HTML::go \
	  lib/Gtk2/Ex/TickerView.pm >TickerView.html

# find files in the dist with mod times this year, but without this year in
# the copyright line
copyright-years-check: $(DISTVNAME).tar.gz
	year=`date +%Y`; \
	tar tvfz $(DISTVNAME).tar.gz \
	| grep $$year- \
	| sed 's:^.*$(DISTVNAME)/::' \
	| (result=0; \
	  while read i; do \
	    case $$i in \
	      '' | */ | COPYING | MANIFEST | META.yml | SIGNATURE \
	      | t/pod.t \
	      | debian/changelog | debian/compat | debian/control) \
	      continue ;; \
	    esac; \
	    if test -e "$(srcdir)/$$i"; then f="$(srcdir)/$$i"; \
	    else f="$$i"; fi; \
	    if ! grep -q "Copyright.*$$year" $$f; then \
	      echo "$$i:1: this file"; \
	      grep Copyright $$f; \
	      result=1; \
	    fi; \
	  done; \
	  exit $$result)

MAKE_FRAG
}
