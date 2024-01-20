CC = gcc
NAME =  

clean:
	rm $(NAME)lexer.ml $(NAME)lexer.mli $(NAME)parser.mli $(NAME)parser.ml  *.cmi *.cmo 


ex: $(NAME)lexer.cmo $(NAME)parser.cmo $(NAME)calc.cmo
	ocamlc -o $@ $^
	
	

$(NAME)lexer.cmo: $(NAME)lexer.ml $(NAME)parser.ml $(NAME)parser.cmi
	ocamlc -c $<

$(NAME)parser.cmo: $(NAME)parser.ml
	ocamlc -c $<

$(NAME)parser.cmo: $(NAME)parser.ml
	ocamlc -c $<


$(NAME)parser.cmi: $(NAME)parser.ml
	ocamlc -c $<



$(NAME)lexer.ml: $(NAME)lexer.mll 
	ocamllex $<

$(NAME)parser.ml: $(NAME)parser.mly
	ocamlyacc $<

$(NAME)parser.mli: $(NAME)parser.mly
	ocamlyacc $<



$(NAME)calc.cmo: $(NAME)calc.ml
	ocamlc -c $<