%{
    #include <stdlib.h>
    #include "code.h"

    #define txmax 100 //符号表容量
    #define al 10   //标识符最长长度

    int tx;   //符号表当前尾指针
    char id[al];

    FILE* fin;      /* 输入源文件 */
    FILE* foutput;  /* 输出出错示意（如有错） */
    char fname[al];
    int err;
    extern int line;

    int yylex();
    void yyerror(char *s);
    void redirectInput(FILE *fin);
%}

%union {
    char *ident;
    int number;
}

%token PLUS MINUS TIMES SLASH LES LEQ GTR GEQ EQL NEQ BECOMES OR AND NOT
%token SEMICOLON LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE COMMA PERIOD
%token IF ELSE WHILE WRITE READ INT BOOL FALSE TRUE
%token SELFADD SELFMIUNS REPEAT UNTIL XOR ODD
%token <ident> ID
%token <number> NUM

%%

program:
	block
	;

block:
	LBRACE decls stmts RBRACE
	;

decls:
	decls decl
    |
	;

decl:
    INT aid SEMICOLON
	| BOOL bid SEMICOLON
	;

aid:
    ID
    ;

bid:
    ID
    ;

stmts:
	stmts stmt
    |
    ;

stmt:
	aid BECOMES aexpr SEMICOLON
    | bid BECOMES bexpr SEMICOLON
    | IF LPAREN bexpr RPAREN SEMICOLON
    | IF LPAREN bexpr RPAREN stmt ELSE SEMICOLON
    | WHILE LPAREN bexpr RPAREN stmt SEMICOLON
    | WRITE aexpr SEMICOLON
    | READ aid SEMICOLON
    | block
	;

aexpr:
    aterm PLUS aterm
    | aterm MINUS aterm
    | aterm
    ;

aterm:
    afactor TIMES afactor
    | afactor SLASH afactor
    | afactor
    ;

afactor:
    aid
    | NUM
    | LPAREN aexpr RPAREN
    ;

bexpr:
    bterm OR bterm
    | bterm
    ;

bterm:
    bterm AND bfactor
    | bfactor
    ;

bfactor:
    bid
    | TRUE
    | FALSE
    | NOT bfactor
    | LPAREN bexpr RPAREN
    | rel
    ;

rel:
    aid comp aexpr
    | NUM comp aexpr
    ;

comp:
    LES | LEQ | GTR | GEQ | EQL | NEQ
    ;

%%

void init() {
    tx = 0;
    err = 0;
}

void yyerror(char *s) {
    err++;
    printf("%s in line %d\n", s, line);
    fprintf(foutput, "%s in line %d\n", s, line);
}

int main() {
    printf("Input cx file?   ");
    scanf("%s", fname);   /* 输入文件名 */

    if ((fin = fopen(fname, "r")) == NULL) {
        printf("Can't open the input file!\n");
        exit(1);
    }
    if ((foutput = fopen("foutput.txt", "w")) == NULL) {
        printf("Can't open the output file!\n");
        exit(1);
    }
    // if ((ftable = fopen("ftable.txt", "w")) == NULL) {
    //     printf("Can't open ftable.txt file!\n");
    //     exit(1);
    // }

    // printf("List object codes?(Y/N)");  /* 是否输出虚拟机代码 */
    // scanf("%s", fname);
    // listswitch = (fname[0]=='y' || fname[0]=='Y');
    //
    // printf("List symbol table?(Y/N)");  /* 是否输出符号表 */
    // scanf("%s", fname);
    // tableswitch = (fname[0]=='y' || fname[0]=='Y');

    redirectInput(fin);
    init();
    yyparse();

    if (err == 0) {
        printf("\n===Parsing success!===\n");
        fprintf(foutput, "\n===Parsing success!===\n");
        // if ((fcode = fopen("fcode.txt", "w")) == NULL) {
        //     printf("Can't open fcode.txt file!\n");
        //     exit(1);
        // }
        // if ((fresult = fopen("fresult.txt", "w")) == NULL) {
        //     printf("Can't open fresult.txt file!\n");
        //     exit(1);
        // }
        // if ((fstack = fopen("fstack.txt", "w")) == NULL) {
        //     printf("Can't open fresult.txt file!\n");
        //     exit(1);
        // }

        // listall();  /* 输出所有代码 */
        // fclose(fcode);

        // interpret();  /* 调用解释执行程序 */
        // fclose(fresult);
        // fclose(fstack);
    } else {
        printf("%d errors in CX program\n", err);
        fprintf(foutput, "%d errors in CX program\n", err);
    }

    // fclose(ftable);
    fclose(foutput);
    fclose(fin);
    return 0;
}
