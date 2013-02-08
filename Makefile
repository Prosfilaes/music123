DESTDIR=/
PREFIX=usr/local

GM=/home/dvdeug/bin/gcc-3.4/bin/gnatmake

all: music123 po-files

music123: music123.adb support_routines.ads support_routines.adb ustring_list.ads vector.adb vector.ads intl.ads intl.adb
	$(GM) -g -gnatf -O2 music123.adb

po-files:
	cd po; ./Make.sh

install: music123.1 music123
	echo "To be used automatically only!"
	echo "If you're doing this by hand, copy them into place by hand."
	sleep 5s
	chmod 755 music123
	chmod 644 music123.1 wavgzplay.sh wavgzplay.sh.1
	strip music123
	cp music123 wavgzplay.sh $(DESTDIR)$(PREFIX)/bin
	cp music123.1 $(DESTDIR)$(PREFIX)/man/man1
	cp music123.pl.1 $(DESTDIR)$(PREFIX)/man/pl/man1/music123.1
	gzip -9 $(DESTDIR)$(PREFIX)/man/man1/music123.1
	gzip -9 $(DESTDIR)$(PREFIX)/man/pl/man1/music123.1
	cp README $(DESTDIR)$(PREFIX)/share/doc/music123
	cp music123rc.conf $(DESTDIR)/etc/music123rc
	cp wavgzplay.sh $(DESTDIR)$(PREFIX)/share/doc/music123/examples
	cp wavgzplay.sh.1 $(DESTDIR)$(PREFIX)/share/doc/music123/examples
	for i in *.mo; do mkdir -p $(DESTDIR)$(PREFIX)/share/locale/`basename $$i .mo`/LC_MESSAGES; cp $$i $(DESTDIR)$(PREFIX)/share/locale/`basename $$i .mo`/LC_MESSAGES/music123.mo; done

clean:
	-rm music123 *.o *.ali *~ b~* *.mo
