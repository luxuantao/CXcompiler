all: prog clean

prog:   cx.l cx.y
	bison -dv cx.y
	flex cx.l
	gcc -o cx lex.yy.c cx.tab.c

clean:
	rm lex.yy.c cx.tab.c cx.tab.h cx.output
