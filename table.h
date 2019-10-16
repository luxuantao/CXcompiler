#include "stdio.h"
#include "string.h"

#define TXMAX 100
#define AL 10
#define LEVMAX 3
#define AMAX 2047
#define STACKSIZE 500

int lev=0;
int tx=0;
int dx=0;
int num;
char id[AL];

enum object{constant, variable, procedur};
enum object kind;

error(int);

struct table1 {
	char name[AL];
	enum object kind;
	int val,level,adr,size;
} table[TXMAX+1];

struct levelReg1 {
    int dx0;
    int tx0;
    int cx0;
} levelReg[LEVMAX+1];

void enter(enum object k) {
    tx = tx+1;
    strcpy(table[tx].name,id);
    table[tx].kind = k;
    switch(k) {
        case constant:
            if (num > AMAX) {
                error(31);
                num=0;
            }
            table[tx].val = num;
            break;
        case variable:
            table[tx].level = lev;		
            table[tx].adr = dx;
            dx++;
            break;
        case procedur:
            table[tx].level = lev;
            break;
    }
}

int position(char id[10]) {
	int i;
	strcpy(table[0].name,id);
	i = tx;
	while (strcmp(table[i].name,id) != 0) {
		i--;
    }
	return i;
}
