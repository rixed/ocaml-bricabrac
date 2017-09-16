all: byte opt
byte: bricabrac.cma
opt: bricabrac.cmxa

NAME = bricabrac

ML_SOURCES = bricabrac.ml

REQUIRES =

include make.common

.PHONY: all opt install uninstall reinstall

$(NAME).cma: $(ML_OBJS)
	$(OCAMLC)   -a -o $@ -package "$(REQUIRES)" $(OCAMLFLAGS) $(ML_OBJS)

$(NAME).cmxa: $(ML_XOBJS)
	$(OCAMLOPT) -a -o $@ -package "$(REQUIRES)" $(OCAMLOPTFLAGS) $(ML_XOBJS)

$(NAME).cmxs: $(ML_XOBJS)
	$(OCAMLOPT) -shared -o $@ -package "$(REQUIRES)" $(OCAMLOPTFLAGS) $(ML_XOBJS)

install: all
	if test -f $(NAME).cmxa ; then extra="$(NAME).cmxa $(NAME).cmx $(NAME).a" ; fi ; \
	ocamlfind install $(NAME) *.cmi $(NAME).cma META $$extra

uninstall:
	ocamlfind remove $(NAME)

reinstall: uninstall install

check: $(NAME).cma $(NAME).cmxa
	$(MAKE) -C tests all opt
	@for t in tests/*.byte tests/*.opt ; do $$t ; done
	@echo Ok

clean-spec:
	$(MAKE) -C tests clean

distclean: clean

-include .depend
