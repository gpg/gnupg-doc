
all: swdb.lst.sig

swdb.lst: swdb.mac
	awk '/^#\+macro:/ {print $$2, $$3}' swdb.mac >swdb.lst

swdb.lst.sig: swdb.lst
	gpg -sbu 0x249B39D24F25E3B6 swdb.lst
