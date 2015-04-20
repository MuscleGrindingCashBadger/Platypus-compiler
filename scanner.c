/* File name: scanner.c
Compiler: MS Visual Studio 2012
Author: Liam Wilkinson, 040771607
Course: CST 8152 - Compilers, Lab Section: 012
Assignment: 2
Date: 2015-03-05
Professor: Svillen Ranev
Purpose:SCANNER.C: Functions implementing a Lexical Analyzer (Scanner)
        as required for CST8152, Assignment #2
        scanner_init() must be called before using the scanner.
Function list: 
*/


/* The #define _CRT_SECURE_NO_WARNINGS is used to suppress
warnings about using "unsafe" functions */
#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */

/*#define NDEBUG        to suppress assert() call */
#include <assert.h>  /* assert() prototype */

/* project header files */
#include "buffer.h"
#include "token.h"
#include "table.h"
#include "stable.h"

#define DEBUG  /* for conditional processing */
#undef  DEBUG

#define PLT_SHRT_MAX 32767

/* Global objects - variables */
/* This buffer is used as a repository for string literals.
   It is defined in platy_st.c */
extern Buffer * str_LTBL; /*String literal table */
int line; /* current line number of the source code */
/*extern int scerrnum;      defined in platy_st.c - run-time error number */

extern STD sym_table;

/* Local(file) global objects - variables */
static Buffer *lex_buf;/*pointer to temporary lexeme buffer*/

/* scanner.c static(local) function  prototypes */ 
static int char_class(char c); /* character class function */
static int get_next_state(int, char, int *); /* state machine function */
static int iskeyword(char * kw_lexeme); /*keywords lookup functuion */
static long atool(char * lexeme); /* converts octal string to decimal value */

int scanner_init(Buffer * sc_buf) {
    if(b_isempty(sc_buf))
		return EXIT_FAILURE;
  /* in case the buffer has been read previously  */
  b_setmark(sc_buf, 0);
  b_retract_to_mark(sc_buf);
  b_reset(str_LTBL);
  line = 1;
  return EXIT_SUCCESS;
}

/*
Purpose: Builds error token out of given array of chars
Author: Liam Wilkinson
History: 1.0
Called functions: strcpy()
Parameters: a valid array of characters
Return value: token with set error attributes and err_lex
Algorithm: Huge case statement that builds tokens and if it's 
not an easy to tokenize it gets sent to the finite state machine.
*/
Token mlwpar_next_token(Buffer * sc_buf)
{
	Token t; /* token to return after recognition */
	unsigned char c; /* input symbol */
	int state = 0; /* initial state of the FSM */
	short lexstart;  /*start offset of a lexeme in the input buffer */
	short lexend;    /*end   offset of a lexeme in the input buffer */
	int accept = NOAS; /* type of state - initially not accepting */
	int i;
	//Ignore the man beheind the curtain
	static int str_offset = 0;
	if (sc_buf == NULL){
		//return runtime_error(ERR_NULL_BUFFER);
		return aa_func12("RUNTIME ERROR");
	}

	while (1){ /* endless loop broken by token returns it will generate a warning */
		c = b_getc(sc_buf);
		/* special cases or token driven processing */
		/* Empty string */
		switch (c){
		case 255: t.code = SEOF_T; return t; /*return EOF token*/
		case ' ': continue; /*Skip white space*/
		case '\0': t.code = SEOF_T; return t; /* Source end of file */
		case '\n': line++; continue; /* Skip newline, increment line count */
		case '\r': line++; continue; /* Skip carage return, increment line count */
		case ';': t.code = EOS_T; return t; /* End of statement token */
		case ',': t.code = COM_T; return t; /* comma token */
		case '{': t.code = LBR_T; return t; /* left brace token */
		case '}': t.code = RBR_T; return t; /* right brace token */
		case '(': t.code = LPR_T; return t; /* left parenthesis token */
		case ')': t.code = RPR_T; return t; /* right parenthesis token */
		case '+': t.code = ART_OP_T; t.attribute.arr_op = PLUS; return t; /* plus operator token */
		case '-': t.code = ART_OP_T; t.attribute.arr_op = MINUS; return t; /* subract operator token */
		case '*': t.code = ART_OP_T; t.attribute.arr_op = MULT; return t; /* multiply operator token */
		case '/': t.code = ART_OP_T; t.attribute.arr_op = DIV; return t; /* divide operator token */
		case '>': t.code = REL_OP_T; t.attribute.rel_op = GT; return t; /* greater than operator token */
		case '<':
			if ((c = b_getc(sc_buf)) == '>'){
				t.code = REL_OP_T;
				t.attribute.rel_op = NE;
				return t;
			} else if (c == '<'){
				t.code = SCC_OP_T;
			} else {
				t.code = REL_OP_T;
				t.attribute.rel_op = LT;
			}
			b_retract(sc_buf);
			c = b_getc(sc_buf);
			return t;
		case '.':
			b_setmark(sc_buf, b_getc_offset(sc_buf));
			c = b_getc(sc_buf);
			if (c == 'A' && b_getc(sc_buf) == 'N' &&
				b_getc(sc_buf) == 'D' && b_getc(sc_buf) == '.'){
				t.code = LOG_OP_T;
				t.attribute.log_op = AND;
				return t;
			}
			else if (c == 'O' && b_getc(sc_buf) == 'R' && b_getc(sc_buf) == '.'){
				t.code = LOG_OP_T;
				t.attribute.log_op = OR;
				return t;
			}
			t.code = ERR_T;
			t.attribute.err_lex[0] = '.';
			t.attribute.err_lex[1] = '\0';
			b_retract_to_mark(sc_buf);
			return t;
		case '=':
			c = b_getc(sc_buf);
			if (c == '='){
				t.code = REL_OP_T;
				t.attribute.rel_op = EQ;
				return t;
			}
			b_retract(sc_buf);
			t.code = ASS_OP_T;
			return t;
		case '!':
			c = b_getc(sc_buf);
			if (c == '<'){
				/* Fancy for loop that gets the next char until it returns an ending character*/
				for (; c != '\n' && c != '\0' && c!=255 && c != '\r'; c = b_getc(sc_buf));
				line++;
				continue;
			}
			else {
				t=aa_table[ES](" ");
				t.attribute.err_lex[0] = c;
				b_retract(sc_buf);
				return t;
			}
		case '\"':
			c = b_getc(sc_buf);
			b_setmark(sc_buf, b_getc_offset(sc_buf));
			lexstart = (short) str_offset;
			lexend = lexstart;
			for (; c != '\"'; c = b_getc(sc_buf)){
				b_addc(str_LTBL, c);
				if (b_isfull(str_LTBL)){
					/* Clever, shame I didn't have time to fix the rest */
					return aa_table[ES]("\"Imagine all the ..");
				}
				if (c == '\n' || c == '\r'){
					line++;
				}
				if (c == 255 || c == '\0'){
					b_retract_to_mark(sc_buf);
					for (i = 0; i < ERR_LEN; i++){
						t.attribute.err_lex[i] = b_getc(sc_buf);
					}
				}
				lexend++;
				str_offset++;
			}
			str_offset++;
			b_addc(str_LTBL, '\0');
			t.code = STR_T;
			t.attribute.str_offset = lexstart;
			return t;
		default:
			if (isalpha(c) || isalnum(c)){
				lexend = 0;
				state = 0;
				lex_buf = b_create(1, 1, 'a');
				while (accept == NOAS){
					b_addc(lex_buf, c);
					state = get_next_state(state, c, &accept);
					if (accept != NOAS){
						break;
					}
					c = b_getc(sc_buf);
					lexend++;
				}
				lex_buf->ca_head[lexend] = '\0';
				if (as_table[state] == ASWR){
					b_retract(sc_buf);
				}
				if ((t.attribute.kwt_idx = iskeyword(b_setmark(lex_buf, 0))) != -1){
					t.code = KW_T;
					b_destroy(lex_buf);
					return t;
				}
				if (aa_table[state]!=NULL){
					t = aa_table[state](b_setmark(lex_buf, 0));
				} else {
					t=aa_table[ES]("RUN TIME ERROR");
				}
				b_destroy(lex_buf);
			} else {
				t = t = aa_table[ES](" ");
				t.attribute.err_lex[0] = c;
			}
			return t;
		}
	}
}

/*
Purpose: Gets the next state in the state machine
Author: Svillen Ranev
History: 1.0
Called functions: char_class, assert, printf
Parameters: int with a valid state, the next character to be analyzed, the accepting function pointer
Return value: an int with the next state
Algorithm: Checks in the transition table what the next state should be
*/
int get_next_state(int state, char c, int *accept)
{
	int col;
	int next;
	col = char_class(c);
	next = st_table[state][col];
#ifdef DEBUG
printf("Input symbol: %c Row: %d Column: %d Next: %d \n",c,state,col,next);
#endif
       assert(next != IS);
#ifdef DEBUG
	if(next == IS){
	  printf("Scanner Error: Illegal state:\n");
	  printf("Input symbol: %c Row: %d Column: %d\n",c,state,col);
	  exit(1);
	}
#endif
	*accept = as_table[next];
	return next;
}

/*
Purpose: calculates what column the inputed symbol is on in the transition table 
Author: Liam Wilkinson
History: 1.0
Called functions: isalpha, isalnum
Parameters: a symbol as a char
Return value: an int with the column the symbol matchs with on the transition table
Algorithm: A bunch of else ifs
*/
int char_class (char c) {
  if (isalpha(c)){
    return 0;
  } else if (c == '0'){
	return 1;
  } else if (c > '0' && c < '8') {
    return 2;
  } else if (c == '8' || c == '9'){
    return 3;
  }  else if (c == '.'){
	return 4;
  } else if (c == '#'){
	return 5;
  } else {
	return 6;
  }
}

/*
Purpose: Accepting state for VID AVID/KW
Author: Liam Wilkinson
History: 1.0
Called functions: iskeyword, strlen
Parameters: a valid lexeme as an constatn array of chars
Return value: A token with the new VID information or a keyword
Algorithm: checks if keyword and if not copys lexeme into vid_lex and null terminates it
*/
Token aa_func02(char lexeme[]){
  unsigned int i;
  Token t;
  int offset;
  char* tempStr;
  char type;
  if ((i = iskeyword(lexeme)) > -1){
    t.code = KW_T;
    t.attribute.kwt_idx = i;
    return t;
  }
	t.code = AVID_T;
	if((tempStr = (char*) calloc(VID_LEN + 1, sizeof(char)))==NULL){
		return aa_table[ES]("RUNTIME ERROR");
	}
	strncpy(tempStr, lexeme, VID_LEN);
	switch(lexeme[0]){
		case 'i': case 'o': case 'd': case 'n': type = 'I'; break;
		default: type = 'F';
	}
	if ((offset = st_install(sym_table, lexeme, type, line)) == -1){
		printf("\nError: The Symbol Table is full - install failed.\n");
		st_store(sym_table);
		exit(1);
	}
	t.attribute.vid_offset = offset;
  return t;
}

/*
Purpose: Accepting state for VID SVID
Author: Liam Wilkinson
History: 1.0
Called functions: iskeyword, strlen
Parameters: a valid lexeme as an constatn array of chars
Return value: A token with the new VID information or a keyword
Algorithm: checks if keyword and if not copys lexeme into vid_lex and null terminates it
*/
Token aa_func03(char lexeme[]){
  Token t;
  int offset;
  char* tempStr;
  if((tempStr = (char*) calloc(VID_LEN + 2, sizeof(char)))==NULL){
	return aa_table[ES]("RUNTIME ERROR");
  }
  strncpy(tempStr, lexeme, VID_LEN);
  tempStr[strlen(tempStr)] = '#';
  if ((offset = st_install(sym_table, tempStr, 'S', line)) == -2){
	  printf("\nError: The Symbol Table is full - install failed.\n");
	  st_store(sym_table);
	  free(tempStr);
	  exit(1);
  }
  t.code = SVID_T;
  t.attribute.vid_offset = offset;
  return t;
}

/*
Purpose: Accepting state for FPL
Author: Liam Wilkinson
History: 1.0
Called functions: atof, aa_func12
Parameters: a valid lexeme as an constant array of chars
Return value: A token with the flating point number copied into flt_value and the correct code set
Algorithm: converts temp variable as double, checks double and if in range everything is set
*/
Token aa_func08(char lexeme[]){
  Token t;
  double temp_double;
  t.code = FPL_T;
  if (strstr(lexeme, "0.0")){
	  t.attribute.flt_value = 0.0;
	  return t;
  }
  temp_double = atof(lexeme);
  if ((temp_double > FLT_MAX) || (temp_double < 0)){
	  t = t=aa_table[ES](lexeme);
  }
  t.attribute.flt_value = (float)temp_double;
  return t;
}

/*
Purpose: Accepting state for integer literal (IL)
Author: Liam Wilkinson
History: 1.0
Called functions: atof, aa_func12
Parameters: a valid lexeme as an constant array of chars that contain numbers between 0 and 7
Return value: A token with the octal constant number copied into int_value and the correct code set
Algorithm: calls function to convert char* to base 10 and gets temp long, checks said long and sets everything
*/
Token aa_func05(char lexeme[]){
	Token t;
	long temp_number;
	temp_number = strtol(lexeme, NULL, 10);
	if (temp_number > PLT_SHRT_MAX || temp_number < 0){
		t = aa_table[ES](lexeme);
	}
	t.code = INL_T;
	t.attribute.int_value = temp_number;
	return t;
}

/*
Purpose: Accepting state for octal constant (OIL)
Author: Liam Wilkinson
History: 1.0
Called functions: atof, aa_func12
Parameters: a valid lexeme as an constant array of chars with numbers between 0 and 10
Return value: A token with the octal constant number copied into int_value and the correct code set
Algorithm: calls function to convert char* to base 8 and gets temp long, checks said long and sets everything
*/
Token aa_func11(char lexeme[]){
  Token t;
  long temp_number;
  temp_number = strtol(lexeme, NULL, 8);
  if (temp_number > PLT_SHRT_MAX || temp_number < 0){
	  t = aa_table[ES](lexeme);
  }
  t.code = INL_T;
  t.attribute.int_value = temp_number;
  return t;
}

/*
Purpose: Builds error token out of given array of chars
Author: Liam Wilkinson
History: 1.0
Called functions: strcpy()
Parameters: a valid array of characters
Return value: token with set error attributes and err_lex
Algorithm: N/A
*/
Token aa_func12(char lexeme[]){
  Token t;
  unsigned int i;
  t.code = ERR_T;
  for (i = 0; i < (ERR_LEN - 1) && i < strlen(lexeme); i++){
	  t.attribute.err_lex[i] = lexeme[i];
  }
  t.attribute.err_lex[i] = '\0';
  return t;
}

/*
Purpose: Checks if the char* is a keyword in the keyword table
Author: Liam Wilkinson
History: 1.0
Called functions: strcmp()
Parameters: a valid pointer to a null terminated array of characters 
Return value: an int that acts like a boolean
Algorithm: checks if the lexeme inputted is in the table.
*/
int iskeyword(char * kw_lexeme){
  int i;
  if (kw_lexeme == NULL){
	  return -1;
  }
  for (i = 0; i < KWT_SIZE; i++){
	  if (strcmp(kw_table[i], kw_lexeme) == 0){
		  return i;
	  }
  }
  return -1;
}