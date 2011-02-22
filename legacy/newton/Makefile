


#BCFLAGS = -thread threads.cma unix.cma
#NCFLAGS = -thread threads.cmxa unix.cmxa


all: 
	ocamlopt.opt unix.cmxa str.cmxa rutils.ml help.ml -o help
#	ocamlc unix.cma str.cma rutils.ml help.ml -o help



#	ocamlc help.cmo

#	ocamlc  -a -o rutils.cma rutils.cmo
#	ocamlopt unix.cmxa -c rutils.mli rutils.ml
#	ocamlopt  -a -o rutils.cmxa rutils.cmx


cleanup: 
	rm -f *.cmi *.cmx *.cmo *.o *~ a.out

clean: cleanup
	rm -f *.cmxa *.cma *.a help

