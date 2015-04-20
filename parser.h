#ifndef PARSER_H_
#define PARSER_H_

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

/*The includes*/
#include "buffer.h"
#include "stable.h"
#include "token.h"

extern Token mlwpar_next_token(Buffer * sc_buf);
extern int line;

/*enum containing keywords, a little fancier that preprocessor*/
enum keywords {
	NO_ATTR = -1,
	ELSE,
	IF,
	INPUT,
	OUTPUT,
	PLATYPUS,
	REPEAT,
	THEN,
	USING
};

/* Normal functions */
void parser(Buffer*);
void gen_incode(char*);
void match(int pr_token_code, int pr_token_attribute);
void syn_printe(void);

/* All the grammers */
void additive_arithmetic_expression(void);
void additive_arithmetic_expression_prime(void);
void arithmetic_expression(void);
void assignment_expression(void);
void assignment_statement(void);
void conditional_expression(void);
void input_statement(void);
void iteration_statement(void);
void logical_and_expression(void);
void logical_and_expression_prime(void);
void logical_or_expression(void);
void logical_or_expression_prime(void);
void multiplicative_arithmetic_expression(void);
void multiplicative_arithmetic_expression_prime(void);
void opt_statements(void);
void output_statement(void);
void primary_a_relational_expression(void);
void primary_arithmetic_expression(void);
void primary_s_relational_expression(void);
void primary_string_expression(void);
void program(void);
void relational_expression(void);
void relational_expression_prime(void);
void relational_expression_prime_string(void);
void selection_statement(void);
void statement(void);
void statements(void);
void statements_prime(void);
void string_expression(void);
void string_expression_prime(void);
void unary_arithmetic_expression(void);
void variable_identifier(void);
void variable_list(void);
void variable_list_prime(void);
void syn_printe(void);

#endif
