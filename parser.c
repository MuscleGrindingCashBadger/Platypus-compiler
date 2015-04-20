/* File name: parser.c
Compiler: MS Visual Studio 2012
Author: Liam Wilkinson, 040771607
Course: CST 8152 - Compilers, Lab Section: 012
Assignment: 4
Date: 2015-04-17
Professor: Svillen Ranev
Purpose: parser recursivly parses input from the token stream scanner.
Function list:  additive_arithmetic_expression, additive_arithmetic_expression_prime, 
	arithmetic_expression, assignment_expression, assignment_statement, conditional_expression, 
	input_statement, iteration_statement, logical_and_expression, logical_and_expression_prime, 
	logical_or_expression, logical_or_expression_prime, multiplicative_arithmetic_expression, 
	multiplicative_arithmetic_expression_prime, opt_statements, output_statement, primary_a_relational_expression,
	primary_arithmetic_expression, primary_s_relational_expression, primary_string_expression, program, 
	relational_expression, relational_expression_prime, relational_expression_prime_string, selection_statement, 
	statement, statements, statements_prime, string_expression, string_expression_prime, unary_arithmetic_expression, 
	variable_identifier, variable_list, variable_list_prime, syn_printe, parser, gen_incode, match, syn_printe
*/

#include "parser.h"

/* Needed globals */
Buffer* sc_buf;
Token lookahead;
int synerrno; /* number of errors */
extern STD sym_table; /* symbol table */
extern int ky_table; 
extern Buffer* str_LTBL; /* buffer */
extern char* kw_table[]; /* keyword table see matching enum in parser.h */

/*
Purpose: implments panic mode error recovery
Author: Liam Wilkinson
History: 1.0 Grew up in Canada eh
Called functions: syn_printe, mlwpar_next_token, exit
Parameters: sync_token_code as an int
Return value: void
Algorithm: while loop to find the next thing that matches */
void syn_eh(int sync_token_code){
	syn_printe();
	synerrno++;
	while(lookahead.code != SEOF_T){
		lookahead = mlwpar_next_token(sc_buf);
		if (lookahead.code == sync_token_code){
			if (lookahead.code != SEOF_T){
				lookahead = mlwpar_next_token(sc_buf);
			}
		return;
		}
	}
	if (sync_token_code != SEOF_T){
		exit(synerrno);
	}
}

/*
Purpose: prints newline after whatever sting it's given
Author: Liam Wilkinson
History: 1.0
Called functions: printf
Parameters: a valid array of characters
Return value: void
Algorithm: N/A */
void gen_incode(char* code){
	printf("%s\n", code);
}

/*
Purpose: checks if the token code and attribute match the look ahead
Author: Liam Wilkinson
History: 1.0
Called functions: syn_eh, syn_printe and mlwpar_next_token
Parameters: pr_token_code and pr_token_attirbute you want to match to the lookahead
Return value: void
Algorithm: A bunch of switch fallthrough and ifs*/
void match(int pr_token_code, int pr_token_attribute){
	if (pr_token_code != lookahead.code){
		syn_eh(pr_token_code);
		return;
	}
	switch(pr_token_code){
		case ART_OP_T: case REL_OP_T: case LOG_OP_T: case KW_T:
			if (pr_token_attribute != lookahead.attribute.get_int){
				break;
			}
		default:
			if (lookahead.code == SEOF_T){
				return;
			}
			lookahead = mlwpar_next_token(sc_buf);
			if (lookahead.code == ERR_T){
				synerrno++;
				syn_printe();
				lookahead = mlwpar_next_token(sc_buf);
			}
			return;
		}
	syn_eh(pr_token_code);
}

/*
Purpose: parses the given program with a little help from it's freinds
Author: Liam Wilkinson
History: 1.0
Called functions: mlwpar_next_token, program, match, gen_incode
Parameters: a buffer pointer with a program loaded in it
Return value: void
Algorithm: starts the parsing tree and ends it */
void parser(Buffer* in_buf){
	sc_buf = in_buf;
	lookahead = mlwpar_next_token(sc_buf);
	program();
	match(SEOF_T, NO_ATTR);
	gen_incode("PLATY: Source file parsed");
}

/*<additive arithmetic expression> ->
	<multiplicative arithmetic expression> <additive arithmetic expression'>
	
	FIRST Set = { AVID_T, FPL_T, INL_T, ( }
	*/
void additive_arithmetic_expression(void){
	multiplicative_arithmetic_expression();
	additive_arithmetic_expression_prime();
}

/*<additive arithmetic expression'> -> 
	+ <multiplicative arithmetic expression><additive arithmetic expression'>
	| - <multiplicative arithmetic expression><additive arithmetic expression'> 
	| E 
	
	FIRST Set = { +, -, E}
	*/
void additive_arithmetic_expression_prime(void){
	if (lookahead.code == ART_OP_T && lookahead.attribute.arr_op != MULT && lookahead.attribute.arr_op != DIV) {
		match(lookahead.code, lookahead.attribute.arr_op);
		multiplicative_arithmetic_expression();
		additive_arithmetic_expression_prime();
		gen_incode("PLATY: Additive arithmetic expression parsed");
	}
}

/*<arithmetic expression> ->
	<opt left parentheses><unary arithmetic expression><opt right parentheses> | <opt left parentheses><additive arithmetic expression><opt right parentheses>
	
	FIRST Set = {-, +, AVID_T, FPL_T, INL_T, ( }
	*/
void arithmetic_expression(void){
		switch (lookahead.code){
			case ART_OP_T:
				switch (lookahead.attribute.arr_op){
					case MULT: case DIV: syn_printe(); return;
				}
				unary_arithmetic_expression();
				break;
			case AVID_T: case FPL_T: case INL_T: case LPR_T:
				additive_arithmetic_expression(); break;
			case EOS_T:	return;
			default: syn_printe(); return;
	}
	gen_incode("PLATY: Arithmetic expression parsed");
}

/*
<assignment expression> ->
	AVID = <arithmetic expression> | SVID = <string expression>

	FIRST Set = { AVID, SVID }
*/
void assignment_expression(void) {
	switch (lookahead.code) {
		/* Determining the path to take based on the first set */
	case AVID_T:
		match(AVID_T, NO_ATTR);
		match(ASS_OP_T, NO_ATTR);
		arithmetic_expression();
		gen_incode("PLATY: Assignment expression (arithmetic) parsed");
		break;
	case SVID_T:
		match(SVID_T, NO_ATTR);
		match(ASS_OP_T, NO_ATTR);
		string_expression();
		gen_incode("PLATY: Assignment expression (string) parsed");
		break;
	default:
		syn_printe();
	}
}

/*<assignment statement> ->
	<assignment expression>;
	
	FIRST Set = { AVID, SVID }
	*/
void assignment_statement(void) {
	assignment_expression();
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Assignment statement parsed");
}

/*<conditional expression> ->
	<logical OR expression>
	
	FIRST Set = { AVID_T, SVID_T, INL_T, SVID_T, STR_T }
	*/
void conditional_expression(void){
	logical_or_expression();
	gen_incode("PLATY: Conditional expression parsed");
}

/*<input statement> ->
	INPUT (<variable list>);
	
	FIRST Set = { INPUT}
	*/
void input_statement(void){
	match(KW_T, INPUT); 
	match(LPR_T, NO_ATTR);
	variable_list();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Input statement parsed");
}

/*<iteration statement> ->
	USING (<assignment expression>, <conditional expression>, <assignment expression>) 
	REPEAT {
		<opt_statements> 
	};
	
	FIRST Set = { USING }
	*/
void iteration_statement(void)
{
	match(KW_T, USING);
	match(LPR_T, NO_ATTR);
	assignment_expression();
	match(COM_T, NO_ATTR);
	conditional_expression();
	match(COM_T, NO_ATTR);
	assignment_expression();
	match(RPR_T, NO_ATTR);
	match(KW_T, REPEAT);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: USING statement parsed");
}

/*<logical AND expression> ->
	<relational expression><logical AND expression'>
	
	FIRST Set = { AVID_T, FPL_T, INL_T, SVID_T, STR_T }
	*/
void logical_and_expression(void) {
	relational_expression();
	logical_and_expression_prime();
}

/*<logical AND expression'> ->
	.AND. <relation expression><logical AND expression'>| E
	
	FIRST Set = { .AND., E }
	*/
void logical_and_expression_prime(void){
	if (lookahead.code == LOG_OP_T && lookahead.attribute.log_op == AND){
		match(LOG_OP_T, AND);
		relational_expression();
		logical_and_expression_prime();
		gen_incode("PLATY: Logical AND expression parsed");
	}
}

/*<logical OR expression> ->
	<logical AND expression><logical OR expression'>
	
	FIRST Set = { AVID_T, FPL_T, INL_T, SVID_T, STR_T}
	*/
void logical_or_expression(void){
	logical_and_expression();
	logical_or_expression_prime();
}

/*<logical OR expression'> ->
	.OR. <logical AND expression><logical OR expression'>
	| E
	
	FIRST Set = { .OR., E}
	*/
void logical_or_expression_prime(void){
	if ((lookahead.code == LOG_OP_T) && (lookahead.attribute.log_op == OR)) {
		match(LOG_OP_T, OR);
		logical_and_expression();
		logical_or_expression_prime();
		gen_incode("PLATY: Logical OR expression parsed");
	}
}

/*<multiplicative arithmetic expression> ->
	<primary arithmetic expression><multiplicative arithmetic expression'>
	
	FIRST Set = {AVID_T, FPL_T, INL_T, ( }
	*/
void multiplicative_arithmetic_expression(void){
	primary_arithmetic_expression();
	multiplicative_arithmetic_expression_prime();
}

/*<multiplicative arithmetic expression'> -> 
	<primary arithmetic expression> <multiplicative arithmetic expression'>
	
	FIRST Set = { *, / , E }
	*/
void multiplicative_arithmetic_expression_prime(void){
	/* LEaves only div and mult */
	if (lookahead.code == ART_OP_T && lookahead.attribute.arr_op != PLUS && lookahead.attribute.arr_op != MINUS) {
		match(lookahead.code, lookahead.attribute.arr_op);
		primary_arithmetic_expression();
		multiplicative_arithmetic_expression_prime();
		gen_incode("PLATY: Multiplicative arithmetic expression parsed");
	}
}

/*<opt_statments> ->
	<statements> | E
	
	FIRST Set = { AVID_T, SVID_T, IF, USING, INPUT, OUTPUT, E }
	*/
void opt_statements(void) {
	switch(lookahead.code){
	case KW_T:
		switch (lookahead.attribute.kwt_idx){
			case PLATYPUS: case ELSE: case THEN: case REPEAT:
				gen_incode("PLATY: Opt_statements parsed"); return;
		}
	case AVID_T: case SVID_T: statements(); break;
	default: gen_incode("PLATY: Opt_statements parsed"); return;
	}
}

/*<output statement> ->
	OUTPUT (<output statement'>);
	
	<output statement'> ->
	<variable list> | <STR_T>

	FIRST(<output statement>) = { OUTPUT }

	FIRST Set = { AVID_T, SVID_T, E }
	*/
void output_statement(void)
{
	match(KW_T, OUTPUT);
	match(LPR_T, NO_ATTR);
	/*I ended up combining this and it's prime in practice*/
	switch (lookahead.code){
		case AVID_T: case SVID_T:
			variable_list();
			break;
		case STR_T:
			match(STR_T, NO_ATTR);
			gen_incode("PLATY: Output list (string literal) parsed");
			break;
		default: gen_incode("PLATY: Output list (empty) parsed"); break;
	}
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: OUTPUT statement parsed");
}

/*<primary a_relational expression> ->
	AVID_T
	| FPL_T
	| INL_T
	
	FIRST Set = { AVID_T, FPL_T, INL_T }
	*/
void primary_a_relational_expression(void){
	switch(lookahead.code){
		case AVID_T: case FPL_T: case INL_T: match(lookahead.code, NO_ATTR); break;
		case LOG_OP_T: break;
		default: syn_printe();
	}
	gen_incode("PLATY: Primary a_relational expression parsed");
}

/*<primary arithmetic expression> ->
	<AVID_T> 
	|<FPL_T>
	|<INL_T>
	|(<arithmetic expression>)
	
	FIRST Set = { AVID_T, FPL_T, INL_T, ( }
	*/
void primary_arithmetic_expression(void) {
	switch (lookahead.code) {
		case AVID_T: case FPL_T: case INL_T:
			match(lookahead.code, lookahead.attribute.arr_op); 
			break;
		case LPR_T:
			match(lookahead.code, lookahead.attribute.arr_op);
			arithmetic_expression();
			match(RPR_T, NO_ATTR);
			break;
		default: syn_printe(); return;
	}
	gen_incode("PLATY: Primary arithmetic expression parsed");
}

/*<primary s_relational expression> ->
	<primary string expression>
	
	FIRST Set = { SVID_T, STR_T } */
void primary_s_relational_expression(void){
	switch (lookahead.code){
		case SVID_T: case STR_T: primary_string_expression(); break;
		default: syn_printe();
	}
	gen_incode("PLATY: Primary s_relational expression parsed");
}

/*<primary string expression> ->
	<SVID_T> | <STR_T>
	
	lol I wonder what the first set is?

	FIRST Set = { SVID_T, STR_T }
	*/
void primary_string_expression(void) {
	switch (lookahead.code) {
		case SVID_T: case STR_T: match(lookahead.code, NO_ATTR); break;
		default: syn_printe();
	}
	gen_incode("PLATY: Primary string expression parsed");
}

/*<program> ->
	PLATYPUS {<opt_statements>}
	
	FIRST Set = { PLATYPUS }
	*/
void program(void){
	match(KW_T, PLATYPUS); 
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	gen_incode("PLATY: Program parsed");
}

/*<relational expression> ->
	<primary a_relational expression><primary a_relational expression'>
	|<primary s_relational expression><primary s_relational expression'>
	
	FIRST Set = { AVID_T, FPL_T, INL_T, SVID_T, STR_T  }
	*/
void relational_expression(void) {
	switch (lookahead.code) {
		case AVID_T: case FPL_T: case INL_T:
			primary_a_relational_expression();
			relational_expression_prime();
			break;
		case SVID_T: case STR_T:
			primary_s_relational_expression();
			relational_expression_prime_string();
			break;
		default: syn_printe();
	}
	gen_incode("PLATY: Relational expression parsed");
}

/*  <relational expression'> ->
	== <primary a_relational expression> 
	  | <>  <primary a_relational expression> 
	  | > <primary a_relational expression> 
	  | < <primary a_relational expression>
	  
	  FIRST Set = {==, <>, <, >}
	  */
void relational_expression_prime(void){
	if (lookahead.code == REL_OP_T){
		switch (lookahead.attribute.rel_op){
			case EQ: case NE: case GT: case LT:
				match(lookahead.code, lookahead.attribute.arr_op);
				primary_a_relational_expression();
				return;
		}
	}
	syn_printe();
}

/* <relational expression prime string> ->
		== <primary s_relational expression> 
		| <> <primary s_relational expression> 
		| > <primary s_relational expression> 
		| < <primary s_relational expression>

		FIRST Set = {==, <>, <, >}
*/
void relational_expression_prime_string(void){
	if (lookahead.code == REL_OP_T){
		switch (lookahead.attribute.rel_op){
			case EQ: case NE: case GT: case LT:
				match(lookahead.code, lookahead.attribute.arr_op);
				primary_s_relational_expression();
				return;
		}
	}
	syn_printe();
}

/*<selection statement> ->
	IF (<conditional expression>) THEN <opt_statments> ELSE { <opt_statements> };
	
	FIST Set = { IF }
	*/
void selection_statement(void){
	match(KW_T, IF);
	match(LPR_T, NO_ATTR);
	conditional_expression();
	match(RPR_T, NO_ATTR);
	match(KW_T, THEN);
	opt_statements();
	match(KW_T, ELSE);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: IF statement parsed");
}

/*<statement> ->
	<assignment statement> 
	|<selection statement>
	|<iteration statement>
	|<input statement>
	|<output statement>
	
	FIRST Set = { AVID_T, SVID_T, IF, USING, INPUT, OUTPUT }
	*/
void statement(void) {
	switch (lookahead.code) {
		case AVID_T: case SVID_T:
		assignment_statement();
		break;
	case KW_T:
		/* I kind of abused case fallthrough for the firsts */
		switch (lookahead.attribute.kwt_idx){
			case IF: selection_statement(); break;
			case USING: iteration_statement(); break;
			case INPUT: input_statement(); break;
			case OUTPUT: output_statement(); break;
		default: 
			syn_printe();
		}
		break;
	default: syn_printe();
	}
}

/*<statements> ->
	<statement><statements'>
	
	FIRST Set = { AVID_T, SVID_T, IF, USING, INPUT, OUTPUT }
	*/
void statements(void){
	statement();
	statements_prime();
}

/*<statements'> ->
	<statement><statements'> | E
	
	FIRST Set = { AVID_T, SVID_T, IF, USING, INPUT, OUTPUT, E }
	*/
void statements_prime(void){
	switch (lookahead.code) {
	case KW_T:
		switch (lookahead.attribute.kwt_idx){
		case PLATYPUS:case ELSE:case THEN:case REPEAT:
			return;
		}
	case AVID_T: case SVID_T:
		statement();
		statements_prime();
		break;
	}
}


/*<string expression> ->
	<primary string expression><string expression'> 
	
	FIRST Set = { SVID_T, STR_T }
	*/
void string_expression(void) {
	primary_string_expression();
	string_expression_prime();
	gen_incode("PLATY: String expression parsed");
}

/*<string expression'>
	<< <primary string expression><string expression'> | E
	
	FIRST Set = { <<, E }
	*/
void string_expression_prime(void) {
	if (lookahead.code == SCC_OP_T) {
		match(SCC_OP_T, NO_ATTR);
		primary_string_expression();
		string_expression_prime();
	}
}

/*<unary arithmetic expression> ->
	- <primary arithmetic expression> 
	| + <primary arithmetic expression>
	
	FIRST Set = { -, + }
	*/
void unary_arithmetic_expression(void){
	switch (lookahead.code){
	case ART_OP_T:
		switch (lookahead.attribute.arr_op){
			case MULT: case DIV: syn_printe(); return;
		}
		match(lookahead.code, lookahead.attribute.arr_op);
		primary_arithmetic_expression();
		gen_incode("PLATY: Unary arithmetic expression parsed");
		break;
	default: syn_printe(); return;
	}
}

/*<variable indentifier> ->
	<arithmetic variable identifier> | <string variable identifier>
	
	FIRST Set = { AVID_T, SVID_T }
	*/
void variable_identifier(void) {
	switch (lookahead.code) {
		case AVID_T: case SVID_T:
			match(lookahead.code, NO_ATTR);
			break;
		default:
			syn_printe();
	}
}

/*<variable list> ->
	<variable identifier><variable list'>
	
	FIRST Set = { AVID_T, SVID_T }
	*/
void variable_list(void) {
	variable_identifier();
	variable_list_prime();
	gen_incode("PLATY: Variable list parsed");
}

/*<variable list'> ->
	,<variable identifier><variable list'> | E
	
	FIRST Set = { ,, E }
	*/
void variable_list_prime(void) {
	/* E */
	if (lookahead.code != COM_T){
		return;
	}
	/* the actual list part */
	match(COM_T, NO_ATTR);
	variable_identifier();
	variable_list_prime();
}

void syn_printe(void){
Token t = lookahead;

printf("PLATY: Syntax error:  Line:%3d\n",line);
printf("*****  Token code:%3d Attribute: ", t.code);
switch(t.code){
	case  ERR_T: /* ERR_T	 0   Error token */
	printf("%s\n",t.attribute.err_lex);
	 break;
	case  SEOF_T: /*SEOF_T	1   Source end-of-file token */
	printf("NA\n" );
	 break;
	case  AVID_T: /* AVID_T	2   Arithmetic Variable identifier token */
	case  SVID_T :/* SVID_T	3  String Variable identifier token */
	printf("%s\n",sym_table.pstvr[t.attribute.get_int].plex);
	 break;
	case  FPL_T: /* FPL_T	 4  Floating point literal token */
	printf("%5.1f\n",t.attribute.flt_value);
	 break;
	case INL_T: /* INL_T	  5   Integer literal token */
	printf("%d\n",t.attribute.get_int);
	 break;
	case STR_T:/* STR_T	 6   String literal token */
	printf("%s\n",b_setmark(str_LTBL,t.attribute.str_offset));
	break;
	
	case SCC_OP_T: /* 7   String concatenation operator token */
	printf("NA\n" );
	break;
	
	case  ASS_OP_T:/* ASS_OP_T  8   Assignment operator token */
	printf("NA\n" );
	break;
	case  ART_OP_T:/* ART_OP_T  9   Arithmetic operator token */
	printf("%d\n",t.attribute.get_int);
	break;
	case  REL_OP_T: /*REL_OP_T  10   Relational operator token */ 
	printf("%d\n",t.attribute.get_int);
	break;
	case  LOG_OP_T:/*LOG_OP_T 11  Logical operator token */
	printf("%d\n",t.attribute.get_int);
	break;
	
	case  LPR_T: /*LPR_T	12  Left parenthesis token */
	printf("NA\n" );
	break;
	case  RPR_T: /*RPR_T	13  Right parenthesis token */
	printf("NA\n" );
	break;
	case LBR_T: /*	14   Left brace token */
	printf("NA\n" );
	break;
	case RBR_T: /*	15  Right brace token */
	printf("NA\n" );
	break;
	
	case KW_T: /*	 16   Keyword token */
	printf("%s\n",kw_table[t.attribute.get_int]);
	break;
	
	case COM_T: /* 17   Comma token */
	printf("NA\n");
	break;
	case EOS_T: /*	18  End of statement *(semi - colon) */
	printf("NA\n" );
	break;	  
	default:
	printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
	}
}