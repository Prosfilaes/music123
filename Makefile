DESTDIR=/
PREFIX=usr/local

MAIN := music123

GM=gnatmake

all: $(MAIN) po-files

.PHONY: $(MAIN)
$(MAIN):
	$(GM) -g -gnatf -gnatwa -gnaty -gnatwe -O2 $(MAIN)

po-files:
	cd po; ./Make.sh

install_strip:install
	strip $(DESTDIR)$(PREFIX)/bin/music123

install: music123.1 music123
	echo "To be used automatically only!"
	echo "If you're doing this by hand, copy them into place by hand."
	sleep 5s
	chmod 755 music123
	chmod 644 music123.1 wavgzplay.sh wavgzplay.sh.1
	cp music123 wavgzplay.sh $(DESTDIR)$(PREFIX)/bin
	cp music123.1 $(DESTDIR)$(PREFIX)/share/man/man1
	cp music123.pl.1 $(DESTDIR)$(PREFIX)/share/man/pl/man1/music123.1
	gzip -9 $(DESTDIR)$(PREFIX)/share/man/man1/music123.1
	gzip -9 $(DESTDIR)$(PREFIX)/share/man/pl/man1/music123.1
	cp README $(DESTDIR)$(PREFIX)/share/doc/music123
	cp music123rc.conf $(DESTDIR)/etc/music123rc
	cp wavgzplay.sh $(DESTDIR)$(PREFIX)/share/doc/music123/examples
	cp wavgzplay.sh.1 $(DESTDIR)$(PREFIX)/share/doc/music123/examples
	for i in *.mo; do mkdir -p $(DESTDIR)$(PREFIX)/share/locale/`basename $$i .mo`/LC_MESSAGES; cp $$i $(DESTDIR)$(PREFIX)/share/locale/`basename $$i .mo`/LC_MESSAGES/music123.mo; done

clean:
	gnatclean $(MAIN)
	$(RM) *~ *.mo
