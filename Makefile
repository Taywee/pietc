PREFIX ?= /usr/local
.PHONY: phony release debug clean

debug: phony
	./configure -d pietc
	ninja

release: pietc

pietc: phony
	./configure pietc
	ninja

clean:
	ninja -tclean

install : pietc
	install -m 0755 -d $(PREFIX)/bin
	strip pietc
	install -m 0755 -t $(PREFIX)/bin pietc

uninstall :
	-rm $(PREFIX)/bin/pietc
