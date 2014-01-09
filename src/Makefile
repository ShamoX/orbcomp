OCAMLC=ocamlc.opt
OCAMLOPT=ocamlopt.opt
OCAMLDEP=ocamldep
OCAMLLEX=ocamllex
LIBOCAMLDIR=/usr/lib/ocaml/3.10.0
INCLUDES=
                 # all relevant -I options here
LIBS=
LIBSOPT=
OCAMLFLAGS=$(INCLUDES) $(LIBS)    # add other options for ocamlc here
OCAMLOPTFLAGS=$(INCLUDES) $(LIBSOPT) # add other options for ocamlopt here

PROJECT=orbcomp

OBJECTSOBJ=vecteur.cmx constantes.cmx atmosphere_nor.cmx frottement.cmx engin.cmx
OBJECTS=vecteur.cmo constantes.cmo atmosphere_nor.cmo frottement.cmo engin.cmo

all: depend $(PROJECT)

all_bc: depend $(PROJECT)_bc

$(PROJECT): $(OBJECTSOBJ) $(PROJECT).ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -o $(PROJECT) $(OBJECTSOBJ) $(PROJECT).ml

$(PROJECT)_bc: $(OBJECTS) $(PROJECT).ml
	$(OCAMLC) $(OCAMLFLAGS) -o $(PROJECT) $(OBJECTS) $(PROJECT).ml

lexer_bc: $(LEXER_BC_OBJS)
	$(OCAMLC) $(OCAMLFLAGS) -o lexer $(LEXER_BC_OBJS)

lexer: $(LEXER_OBJS)
	$(OCAMLOPT) $(OCAMLFLAGS) -o lexer $(LEXER_OBJS)


# Common rules

.SUFFIXES: .mll .ml .mli .cmo .cmi .cmx

.mll.ml:
	$(OCAMLLEX) $(OCAMLFLAGS) -c $<

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

# Clean up
clean:
	rm -f lexer gateway gateway_bc
	rm -f *.cm* *.o *.a
	rm $(PROJECT)

# Dependencies
depend: .depend
.depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

include .depend
