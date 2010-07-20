VERSION=`head -1 debian/changelog | sed -Ee 's/.*\(([^\)]+)\).*/\1/'`
ERLDIR=`erl -eval 'io:format("~s", [code:root_dir()])' -s init stop -noshell`/lib/vkontakte-$(VERSION)
DESTROOT:=$(CURDIR)/debian/erlang-vkontakte
DEBIANREPO=/apps/erlyvideo/debian/public

all: compile

compile:
	erl -make
	
clean:
	rm -fv ebin/*.beam
	rm -fv erl_crash.dump

test:
	@erl -pa ebin -s vkontakte test -noshell -noinput -s init stop


install: compile
	mkdir -p $(DESTROOT)$(ERLDIR)/ebin
	mkdir -p $(DESTROOT)$(ERLDIR)/src
	mkdir -p $(DESTROOT)$(ERLDIR)/priv
	mkdir -p $(DESTROOT)$(ERLDIR)/include
	install -c -m 644 ebin/*.beam ebin/*.app $(DESTROOT)$(ERLDIR)/ebin/
	install -c -m 644 src/* $(DESTROOT)$(ERLDIR)/src/
	install -c -m 644 priv/vkontakte.conf.sample $(DESTROOT)$(ERLDIR)/priv/vkontakte.conf.sample


debian:
	dpkg-buildpackage -rfakeroot -D -i -I -S -sa
	debuild -us -uc
	cp ../erlang-vkontakte_$(VERSION)*.deb $(DEBIANREPO)/binary/
	rm ../erlang-vkontakte_$(VERSION)*
	(cd $(DEBIANREPO)/..; ./update)

.PHONY: debian