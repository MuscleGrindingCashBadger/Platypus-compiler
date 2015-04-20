/* File name: stable.h
Compiler: MS Visual Studio 2012
Author: Liam Wilkinson, 040771607
Course: CST 8152 - Compilers, Lab Section: 012
Assignment: 3
Date: 2015-03-22
Professor: Svillen Ranev
Purpose: Stores and provides functions for the stable database
Function list: st_install, st_update_type, st_update_value, st_get_type, st_sort, 
		st_create, st_lookup, st_destroy, st_print, st_store, st_setsize, st_incoffset
*/
#ifndef STABLE_H_
#define STABLE_H_

#include "buffer.h"
#include "stdlib.h"
#include "string.h"
#include "stdio.h"

#define DEFAULT_MASK 0xfff8
#define UPDATE_MASK 0x0001
#define INT_MASK 0x0004
#define FLOAT_MASK 0x0002
#define STRING_MASK 0x0006
#define DEFAULT_UPDATE_MASK 0xfff6

typedef union InitialValue {
	int int_val;
	float fpl_val;
	int str_offset;
} InitialValue;

typedef struct SymbolTableVidRecord{
	unsigned short status_field;
	char* plex;
	int o_line;
	InitialValue i_value;
	size_t reserved;
} STVR;

typedef struct SymbolTableDescriptor {
	STVR* pstvr;
	int st_size;
	int st_offset;
	Buffer* plsBD;
} STD;

int st_install(STD, char*, char, int);
int st_update_type(STD, int, char);
int st_update_value(STD, int, InitialValue);
char st_get_type (STD, int);
int st_sort(STD, char);
STD st_create(int);
int st_lookup(STD, char*);
void st_destroy(STD sym_table);
int st_print(STD);
int st_store(STD sym_table);

#endif
