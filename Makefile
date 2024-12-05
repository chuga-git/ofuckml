all: build clean

build:
	ocamlopt -o ofuckml ofuck.ml

clean:
	rm -f *.cmi *.cmx *.o