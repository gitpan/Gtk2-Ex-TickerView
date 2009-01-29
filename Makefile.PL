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

use 5.008;
use strict;
use warnings;
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
                    'Gtk2' => '1.180',

                    # want version 2 for its Gtk2::Buildable support
                    'Gtk2::Ex::CellLayout::Base' => 2,

                    # my Gtk2-Ex-WidgetBits
                    'Gtk2::Ex::SyncCall' => 0,

                    # my Glib-Ex-ObjectBits
                    'Glib::Ex::SignalIds' => 0,
                    'Glib::Ex::SourceIds' => 0,

                    # Extras for the devel/ directory development programs
                    #   
                    #   
                    #
                   },
   clean        => { FILES => "TickerView.html" },

   EXTRA_META => <<'HERE',
optional_features:
    maximum_tests:
        description: Have "make test" do as much as possible.
        requires:
            Test::Pod: 1.00
            Test::Weaken: 1.002
            Gtk2: 0
    maximum_examples:
        description: Be able to run all the example programs.
        requires:
            Finance::Quote: 0
            POE: 0
            POE::Loop::Glib: 0
            POE::Component::Client::HTTP: 0
            HTTP::Request: 0
            URI::Escape: 0
            Gtk2: 0
    maximum_devel:
        description: Be able to run all the development programs.
        requires:
            Glib::Ex::ConnectProperties: 0
            Devel::StackTrace: 0
            Devel::FindRef: 0
            Devel::Peek: 0
no_index:
    directory:
        - devel
resources:
    homepage: http://www.geocities.com/user42_kevin/gtk2-ex-tickerview/index.html
    license: http://www.gnu.org/licenses/#GPL
HERE
  );


sub MY::postamble {
  return <<'MAKE_FRAG';

TickerView.html: lib/Gtk2/Ex/TickerView.pm Makefile
	-perl -MPod::Simple::HTML -e Pod::Simple::HTML::go \
	  lib/Gtk2/Ex/TickerView.pm \
	| sed 's!http://search.cpan.org/perldoc?Glib%3A%3A\([^"]*\)!http://gtk2-perl.sourceforge.net/doc/pod/Glib/\1.html!' \
	| sed 's!http://search.cpan.org/perldoc?Gtk2%3A%3A\([^"%]*\)"!http://gtk2-perl.sourceforge.net/doc/pod/Gtk2/\1.html"!' \
	 >TickerView.html

myman:
	mv MANIFEST MANIFEST.old
	touch SIGNATURE
	(make manifest 2>&1; diff -u MANIFEST.old MANIFEST; rm MANIFEST.old) |less

LINT_FILES = $(TO_INST_PM) t/*.t examples/*.pl devel/*.pl
lint:
	for i in $(LINT_FILES); do perl -MO=Lint $$i; done
pc:
	-podchecker $(LINT_FILES)
	perlcritic $(LINT_FILES)
unused:
	for i in $(LINT_FILES); do \
	  perl -Mwarnings::unused=-lexical \
	    -MO=Lint,no-undefined-subs,no-context \
	    -I lib $$i; \
	done

# find files in the dist with mod times this year, but without this year in
# the copyright line
copyright-years-check:
	year=`date +%Y`; \
	tar tvfz $(DISTVNAME).tar.gz \
	| grep $$year- \
	| sed 's:^.*$(DISTVNAME)/::' \
	| (result=0; \
	  while read i; do \
	    case $$i in \
	      '' | */ \
	      | debian/changelog | debian/compat \
	      | t/pod.t \
	      | COPYING | MANIFEST | SIGNATURE | META.yml) \
	      continue ;; \
	    esac; \
	    if test -e "$(srcdir)/$$i"; then f="$(srcdir)/$$i"; \
	    else f="$$i"; fi; \
	    if ! grep -q "Copyright.*$$year" $$f; then \
	      echo "$$i":"1: this file"; \
	      grep Copyright $$f; \
	      result=1; \
	    fi; \
	  done; \
	  exit $$result)

debug-constants-check:
	if egrep -n 'DEBUG => [^0]' $(TO_INST_PM); then exit 1; else exit 0; fi

diff-prev:
	rm -rf diff.tmp
	mkdir diff.tmp
	cd diff.tmp \
	&& tar xfz ../$(NAME)-`expr $(VERSION) - 1`.tar.gz \
	&& tar xfz ../$(NAME)-$(VERSION).tar.gz
	-cd diff.tmp; diff -ur $(NAME)-`expr $(VERSION) - 1` \
	                       $(NAME)-$(VERSION) >tree.diff
	-less diff.tmp/tree.diff
	rm -rf diff.tmp

MAKE_FRAG
}
