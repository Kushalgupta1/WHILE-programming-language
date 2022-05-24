all:
	ml-lex WhileLang.lex
	ml-yacc WhileLang.yacc
	sml loader.sml
clean:
	rm WhileLang.yacc.sig WhileLang.yacc.sml WhileLang.yacc.desc WhileLang.lex.sml
