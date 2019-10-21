%{
    #include <stdio.h>
    #include <string.h>
    #include <stdlib.h>

    #define true 1
    #define false 0
    #define bool int
    
    #define TXMAX 100 //符号表容量
    #define AL 10 //标识符最长长度
    #define LEVMAX 5 //嵌套深度
    #define AMAX 2047 //地址上界
    #define STACKSIZE 500 //栈元素上界
    #define CXMAX 200 //最多的pcode条数

    enum object {
        constant, 
        variable, 
        procedure
    };

    enum vartype {
        inttype,
        chartype,
        booltype
    };

    /* 符号表结构 */
    struct table1 {
        char name[AL]; /* 名字 */
        enum object kind;  /* 类型：const，var，procedure */
        int val; /* 数值，仅const使用 */
        int level;
        int adr; /* 地址，仅const不使用 */
        int size;
        enum vartype idtype;
    } table[TXMAX];

    enum fct {lit, opr, lod, sto, cal, ini, jmp, jpc};

    struct instruction {
        enum fct f;
        int l;
        int a;
    } code[CXMAX];

    int tx; //符号表当前尾指针
    int cx; // pcode索引
    int px; // 嵌套过程索引表proctable的指针
    int lev; // 层次记录
    int proctable[5]; // 嵌套过程索引表
    char id[AL];
    int num;
    bool listswitch;   /* 显示虚拟机代码与否 */
    bool tableswitch;  /* 显示符号表与否 */
    int dx;

    FILE* fin;      /* 输入源文件 */
    FILE* ftable;   /* 输出符号表 */
    FILE* fcode;    /* 输出虚拟机代码 */
    FILE* foutput;  /* 输出出错示意（如有错） */
    FILE* fresult;  /* 输出执行结果 */
    FILE* fstack;   /* 输出每一步栈的结果 */
    int err; // 程序中的错误个数
    extern int line;

    char category[10]; //判断当前id是否是整形还是字符类型还是布尔类型

    void init();
    void enter(enum object k);
    int position(char* s);
    void setdx(int n);
    void gen(enum fct x, int y, int z);
    void listall();
    void displaytable();
    void interpret();
    int base(int l, int *s, int b);
    
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
%token IF ELSE WHILE WRITE READ INT BOOL CHAR CONST FALSE TRUE 
%token SELFADD SELFMIUNS REPEAT UNTIL XOR ODD 
%token BEG END CALL THEN DO PROC VAR
%token <ident> IDENT
%token <number> NUMBER

%type <number> ident
// %type <number> vardecl varlist vardef
// %type <number> get_table_addr get_code_addr

%%

program:
    block
    ;

block:
    LBRACE
    constdecl
    statements
    RBRACE
	;

/*  常量声明 */
constdecl: 
    CONST constlist SEMICOLON
    |
    ;

/* 常量声明列表 */
constlist: 
    constdef 
    | constlist COMMA constdef 
    ;

/* 单个常量 */
constdef:
    IDENT BECOMES NUMBER
    {        
        strcpy(id, $1);   
        num = $3;
        enter(constant);
    }
    ;

/*  语句 */
statements:
    statements statement
    |
    ;

statement:
    writestm 
    ;

/* 写语句 */
writestm: 
    WRITE LPAREN writeexplist RPAREN SEMICOLON
    ; 

/* 一个或多个写语句的表达式 */
writeexplist:
    expression
    {
        gen(opr, 0, 14);
        gen(opr, 0, 15);
    }
    | writeexplist COMMA expression
    {
        gen(opr, 0, 14);
        gen(opr, 0, 15);
    }
    ;

/* 表达式 */
expression: 
    PLUS term
    | MINUS term
    {
        gen(opr, 0, 1);
    }
    | term             
    | expression PLUS term
    {
        gen(opr, 0, 2);
    }
    | expression MINUS term
    {
        gen(opr, 0, 3);
    }
    ;

/* 项 */
term: 
    factor
    | term TIMES factor
    {
        gen(opr, 0, 4);
    }
    | term SLASH factor
    {
        gen(opr, 0, 5);
    }  
    ;

/* 因子 */
factor: 
    ident
    { 
        if ($1 == 0)
            yyerror("Symbol does not exist");
        else
        {
            if (table[$1].kind == procedure)
                yyerror("Symbol should not be a procedure");
            else
            {
                if(table[$1].kind == constant)
                    gen(lit, 0, table[$1].val);
                else if(table[$1].kind == variable)
                    gen(lod, lev - table[$1].level, table[$1].adr);
            }
        }
    }   
    | NUMBER 
    {
        gen(lit, 0, $1);
    }
    | LPAREN expression RPAREN
    ;

ident: 
    IDENT 
    {
        $$ = position($1); 
    }
    ;

%%

void yyerror(char *s) {
    err++;
    printf("%s in line %d\n", s, line);
    fprintf(foutput, "%s in line %d\n", s, line);
}

void init() {
    tx = 0;
    cx = 0;
    px = 0;  
    lev = 0;   
    proctable[0] = 0;
    num = 0;
    err = 0;
}

/* 在符号表中加入一项 */
void enter(enum object k) {
    tx = tx + 1;
    strcpy(table[tx].name, id);
    table[tx].kind = k;
    switch(k) {
    case constant:
        table[tx].val = num;
        break;
    case variable:
        table[tx].level = lev;
        break;
    case procedure:
        table[tx].level = lev;
        break;
    }
}

/* 查找标识符在符号表中的位置 */
int position(char id[]) {
    int i;
    strcpy(table[0].name, id);
    i = tx;
    while (strcmp(table[i].name,id) != 0) {
        i--;
    }
    return i;
}

/* 为本层变量分配相对地址，从3开始分配 */
void setdx(int n) {
    int i;
    for(i = 1; i <= n; i++) {
        table[tx - i + 1].adr = n - i + 3;
    }
}

/* 生成虚拟机代码 */
void gen(enum fct x, int y, int z) {
    if (cx >= CXMAX) {
        printf("Program is too long!\n"); /* 生成的虚拟机代码程序过长 */
        exit(1);
    }
    if (z >= AMAX) {
        printf("Displacement address is too big!\n"); /* 地址偏移越界 */
        exit(1);
    }
    code[cx].f = x;
    code[cx].l = y;
    code[cx].a = z;
    cx++;
}

/* 输出所有目标代码  */
void listall() {
    int i;
    char name[][5] = {
        {"lit"},{"opr"},{"lod"},{"sto"},{"cal"},{"int"},{"jmp"},{"jpc"},{"loa"},{"sta"},{"hod"},
        {"cpy"},{"jpe"},{"ext"},{"cla"},{"tss"},{"tsl"}
    };
    if (listswitch) {
        for (i = 0; i < cx; i++) {
            printf("%d %s %d %d\n", i, name[code[i].f], code[i].l, code[i].a);
            fprintf(fcode,"%d %s %d %d\n", i, name[code[i].f], code[i].l, code[i].a);
        }
        printf("\n");
    }
}

/* 输出符号表 */
void displaytable() {
    int i;
    if (tableswitch) {
        for (i = 1; i <= tx; i++) {
            switch (table[i].kind)
            {
            // case array:
            //     printf("    %d array %s ", i, table[i].name);
            //     printf("lev=%d addr=%d type=%d len=%d\n", table[i].level, table[i].adr, table[i].idtype, table[i].arraylen);
            //     fprintf(ftable, "    %d var   %s ", i, table[i].name);
            //     fprintf(ftable, "lev=%d addr=%d\n", table[i].level, table[i].adr);
            //     break;
            case constant:
                printf("    %d const %s ", i, table[i].name);
                printf("val=%d\n", table[i].val);
                fprintf(ftable, "    %d const %s ", i, table[i].name);
                fprintf(ftable, "val=%d\n", table[i].val);
                break;
            case variable:
                printf("    %d var   %s ", i, table[i].name);
                printf("lev=%d addr=%d type=%d \n", table[i].level, table[i].adr,  table[i].idtype);
                fprintf(ftable, "    %d var   %s ", i, table[i].name);
                fprintf(ftable, "lev=%d addr=%d\n", table[i].level, table[i].adr);
                break;
            case procedure:
                printf("    %d proc  %s ", i, table[i].name);
                printf("lev=%d addr=%d size=%d\n", table[i].level, table[i].adr, table[i].size);
                fprintf(ftable,"    %d proc  %s ", i, table[i].name);
                fprintf(ftable,"lev=%d addr=%d size=%d\n", table[i].level, table[i].adr, table[i].size);
                break;
            }
        }
        printf("\n");
        fprintf(ftable, "\n");
    }
}

/* 解释程序 */
void interpret() {
	int p = 0; /* 指令指针 */
    int b = 1; /* 指令基址 */
    int t = 0; /* 栈顶指针 */
    struct instruction i; /* 存放当前指令 */
    int s[STACKSIZE]; /* 栈 */

	printf("Start CX interpret\n");
 	fprintf(fresult,"Start CX interpret\n");
	s[0] = 0;
	s[1] = 0;
	s[2] = 0;
	s[3] = 0;
	do {
		i = code[p];  /* 读当前指令 */
        p = p + 1;
        switch (i.f)
        {
        case lit: /* 将常量a的值取到栈顶 */
            t = t + 1;
            s[t] = i.a;
            break;
        case opr: /* 数学、逻辑运算 */
            switch(i.a) {
            case 0:  /* 函数调用结束后返回 */
                t = b - 1;
                p = s[t + 3];
                b = s[t + 2];
                break;
            case 1: /* 栈顶元素取反 */
                s[t] = -s[t];
                break;
            case 2: /* 次栈顶项加上栈顶项，退两个栈元素，相加值进栈 */
                t = t - 1;
                s[t] = s[t] + s[t + 1];
                break;
            case 3:/* 次栈顶项减去栈顶项 */
                t = t - 1;
                s[t] = s[t] - s[t + 1];
                break;
            case 4:/* 次栈顶项乘以栈顶项 */
                t = t - 1;
                s[t] = s[t] * s[t + 1];
                break;
            case 5:/* 次栈顶项除以栈顶项 */
                t = t - 1;
                s[t] = s[t] / s[t + 1];
                break;
            case 6:/* 栈顶元素的奇偶判断 */
                s[t] = s[t] % 2;
                break;
            case 8:/* 次栈顶项与栈顶项是否相等 */
                t = t - 1;
                s[t] = (s[t] == s[t + 1]);
                break;
            case 9:/* 次栈顶项与栈顶项是否不等 */
                t = t - 1;
                s[t] = (s[t] != s[t + 1]);
                break;
            case 10:/* 次栈顶项是否小于栈顶项 */
                t = t - 1;
                s[t] = (s[t] < s[t + 1]);
                break;
            case 11:/* 次栈顶项是否大于等于栈顶项 */
                t = t - 1;
                s[t] = (s[t] >= s[t + 1]);
                break;
            case 12:/* 次栈顶项是否大于栈顶项 */
                t = t - 1;
                s[t] = (s[t] > s[t + 1]);
                break;
            case 13: /* 次栈顶项是否小于等于栈顶项 */
                t = t - 1;
                s[t] = (s[t] <= s[t + 1]);
                break;
            case 14:/* 栈顶值输出 */
                printf("%d", s[t]);
                fprintf(fresult, "%d", s[t]);
                t = t - 1;
                break;
            case 15:/* 输出换行符 */
                printf("\n");
                fprintf(fresult,"\n");
                break;
            case 16:/* 读入一个输入置于栈顶 */
                t = t + 1;
                printf("input a number");
                fprintf(fresult, "input a number");
                scanf("%d", &(s[t]));
                fprintf(fresult, "%d\n", s[t]);           
                break;
            }
            break;	
        case lod: /* 取相对当前过程的数据基地址为a的内存的值到栈顶 */
            t = t + 1;
            s[t] = s[base(i.l, s, b) + i.a];
            break;
        case sto: /* 栈顶的值存到相对当前过程的数据基地址为a的内存 */
            s[base(i.l, s, b) + i.a] = s[t];
            t = t - 1;
            break;
        case cal: /* 调用子过程 */
            s[t + 1] = base(i.l, s, b); /* 将父过程基地址入栈，即建立静态链 */
            s[t + 2] = b; /* 将本过程基地址入栈，即建立动态链 */
            s[t + 3] = p; /* 将当前指令指针入栈，即保存返回地址 */
            b = t + 1;  /* 改变基地址指针值为新过程的基地址 */
            p = i.a;  /* 跳转 */
            break;
        case ini: /* 在数据栈中为被调用的过程开辟a个单元的数据区 */
            t = t + i.a;
            break;
        case jmp: /* 直接跳转 */
            p = i.a;
            break;
        case jpc: /* 如果栈顶等于0条件跳转 */
            if (s[t] == 0)
                p = i.a;
            t = t - 1;
            break;
        }
    } while (p != 0);
    printf("End CX interpret\n");
    fprintf(fresult, "End CX interpret\n");
}

/* 通过过程基址求上ｌ层过程的基址 */
int base(int l, int s[], int b) {
    int b1 = b;
    while (l > 0) {
        b1 = s[b1];
        l = l - 1;
    }
    return b1;
}

int main(int argc, char *argv[]) {
    int i;
    if (argc > 1) {
        for (i = 1; i < argc; i++) {
            if (strcmp(argv[i], "-c") == 0) {  /* 是否输出虚拟机代码 */
                listswitch = true;
            } else if (strcmp(argv[i], "-t") == 0) {  /* 是否输出符号表 */
                tableswitch = true;
            }
        }
    }
    if ((fin = fopen(argv[argc - 1], "r")) == NULL) {
        printf("Can't open the input file!\n");
        exit(1);
    }
    if ((foutput = fopen("foutput.txt", "w")) == NULL) {
        printf("Can't open the output file!\n");
        exit(1);
    }
    if ((ftable = fopen("ftable.txt", "w")) == NULL) {
        printf("Can't open ftable.txt file!\n");
        exit(1);
    }

    redirectInput(fin);
    init();
    yyparse();

    if (err == 0) {
        printf("\n===Parsing success!===\n\n");
        fprintf(foutput, "\n===Parsing success!===\n");
        if ((fcode = fopen("fcode.txt", "w")) == NULL) {
            printf("Can't open fcode.txt file!\n");
            exit(1);
        }
        if ((fresult = fopen("fresult.txt", "w")) == NULL) {
            printf("Can't open fresult.txt file!\n");
            exit(1);
        }
        if ((fstack = fopen("fstack.txt", "w")) == NULL) {
            printf("Can't open fresult.txt file!\n");
            exit(1);
        }

        listall();  /* 输出所有代码 */
        fclose(fcode);

        interpret();  /* 调用解释执行程序 */
        fclose(fresult);
        fclose(fstack);
    } else {
        printf("%d errors in CX program\n", err);
        fprintf(foutput, "%d errors in CX program\n", err);
    }

    fclose(ftable);
    fclose(foutput);
    fclose(fin);
    return 0;
}
