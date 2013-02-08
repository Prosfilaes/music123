DESTDIR=/
PREFIX=usr/local

all: music123.1 music123

music123.1: music123.adb
	head -n 73 music123.adb | cut -c 4-100 > music123.1

music123: music123.adb natural_vector.adb natural_vector.ads support_routines.ads support_routines.adb ustring_list.ads vector.adb vector.ads intl.ads intl.adb
	gnatmake -g -gnatf -O2 music123.adb

install: music123.1 music123
	echo "To be used automatically only!"
	echo "If you're doing this by hand, copy them into place by hand."
	sleep 5s
	chmod 755 music123
	chmod 644 music123.1 wavgzplay.sh wavgzplay.sh.1
	cp music123 wavgzplay.sh $(DESTDIR)$(PREFIX)/bin
	cp music123.1 $(DESTDIR)$(PREFIX)/man/man1
	gzip -9 $(DESTDIR)$(PREFIX)/man/man1/music123.1
	cp README $(DESTDIR)$(PREFIX)/share/doc/music123
	cp music123rc.conf $(DESTDIR)/etc/music123rc
	cp wavgzplay.sh $(DESTDIR)$(PREFIX)/share/doc/music123/examples
	cp wavgzplay.sh.1 $(DESTDIR)$(PREFIX)/share/doc/music123/examples

clean:
	-rm music123.1 music123.1 music123 *.o *.ali *~ b~*
