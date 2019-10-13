%{
	#include "stdio.h"
	#include "cx.tab.h"

	int line = 1;
	int yywrap();
	void yyerror(char *);
	char msg[100];
%}

whitespace  ([ \t\n])+
ID          [A-Za-z][A-Za-z0-9]*
NUM         ([1-9][0-9]*)|0

%%

[ \t]	   ;
[\n]	   {line++;}

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
"repeat"   {return REPEAT;}
"until"    {return UNTIL;}
"XOR"      {return XOR;}
"ODD"      {return ODD;}

{ID}       {
				yylval.ident = (char *)malloc(strlen(yytext) + 1);
				strcpy(yylval.ident, yytext);
				return ID;
		   }

{NUM}      {
				yylval.number = atoi(yytext);
				return NUM;
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
