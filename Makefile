GHC ?= ghc
FLAGS?= -O2 -Wall

all:
	${GHC} Nyancat.hs -o nyancat ${FLAGS}

install:
	cp nyancat /usr/bin -v
	cp nyancat.1 /usr/share/man/man1/
	mkdir -p /usr/share/nyancat
	cp -r res/* /usr/share/nyancat/

uninstall:
	rm /usr/bin/nyancat
	rm /usr/share/man/man1/nyancat.1
	rm -r /usr/share/nyancat

clean:
	rm nyancat
