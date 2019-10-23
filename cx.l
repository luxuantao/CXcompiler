%{
	#include "stdio.h"
	#include "cx.tab.h"

	int line = 1;
	int yywrap();
	void yyerror(char *);
	char msg[100];
	void comment();
%}

whitespace  ([ \t\n])+
ID          [A-Za-z][A-Za-z0-9]*
NUM         ([1-9][0-9]*)|0

%%

[ \t]	   {;}
[\n]	   {line++;}

"/*"       {comment();} 
"+"        {return PLUS;}
"-"        {return MINUS;}
"*"        {return TIMES;}
"/"        {return SLASH;}
"<"        {return LES;}
"<="       {return LEQ;}
">"        {return GTR;}
">="       {return GEQ;}
"=="       {return EQL;}
"!="       {return NEQ;}
"="        {return BECOMES;}
"||"       {return OR;}
"&&"       {return AND;}
"!"        {return NOT;}
";"        {return SEMICOLON;}
"("        {return LPAREN;}
")"        {return RPAREN;}
"["        {return LBRACKET;}
"]"        {return RBRACKET;}
"{"        {return LBRACE;}
"}"        {return RBRACE;}
","        {return COMMA;}
"."        {return PERIOD;}
"++"	   {return SELFADD;}
"--"	   {return SELFMIUNS;}

"true"     {return TRUE;}
"false"    {return FALSE;}
"if"       {return IF;}
"else"     {return ELSE;}
"while"    {return WHILE;}
"write"    {return WRITE;}
"read"     {return READ;}
"int"      {return INT;}
"bool"     {return BOOL;}
"char"     {return CHAR;}
"const"    {return CONST;}
"repeat"   {return REPEAT;}
"until"    {return UNTIL;}
"^"        {return XOR;}
"%"        {return MOD;}
"ODD"      {return ODD;}
"call"     {return CALL;}
"then"     {return THEN;}
"do"       {return DO;}
"func"     {return PROC;}
"var"      {return VAR;}

{ID}    {
				yylval.ident = (char *)malloc(strlen(yytext) + 1);
				strcpy(yylval.ident, yytext);
				return IDENT;
		   }

{NUM}   {
				yylval.number = atoi(yytext);
				return NUMBER;
		   }

.          {
				sprintf(msg, "yyerror: unexpected character %s", yytext);
                yyerror(msg);
           }

%%

void redirectInput(FILE *input) {
	yyin = input;
}

int yywrap() {
    return 1;
}

void comment() {
    char c, c1;
loop:
    while ((c = input()) != '*' && c != 0) {
        // putchar(c);
	}
    if ((c1 = input()) != '/' && c != 0) {
        unput(c1);
        goto loop;
    }
    if (c != 0) {
        // putchar(c1);
	}
}