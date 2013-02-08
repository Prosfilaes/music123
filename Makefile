DESTDIR=/

all: music123.1 music123

music123.1: music123.adb
	head -n 69 music123.adb | cut -c 4-100 > music123.1

music123: music123.adb natural_vector.adb natural_vector.ads support_routines.ads support_routines.adb ustring_list.ads vector.adb vector.ads
	gnatmake -g -gnatf -O2 -I/usr/include/gtkada music123.adb

install: music123.1 music123
	echo "To be used automatically only!"
	echo "If you're doing this by hand, copy them into place by hand."
	sleep 5s
	chmod 755 music123 wavgzplay.sh
	chmod 644 music123.1
	cp music123 wavgzplay.sh $(DESTDIR)/usr/bin
	cp music123.1 $(DESTDIR)/usr/man/man1
	cp README $(DESTDIR)/usr/share/doc/music123
	cp music123rc.conf $(DESTDIR)/etc/music123rc	

clean:
	-rm music123.1 music123.1 music123 *.o *.ali *~ b~*
