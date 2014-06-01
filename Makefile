all: lexer.hs parser.hs
	ghc main.hs parser.hs lexer.hs types.hs visitor.hs -o clikeparser

lexer.hs: lexer.x
	alex lexer.x

lexer: lexer.hs
	ghc lexer.hs

lexerclean:
	rm lexer.hi lexer.o

parser.hs: parser.y
	happy parser.y

parser: lexer.hs types.hs parser.hs
	ghc types.hs lexer.hs parser.hs

parserclean:
	rm parser.hi parser.o

clean: lexerclean parserclean
	rm *.o
	rm *.hi
	rm clikeparser
