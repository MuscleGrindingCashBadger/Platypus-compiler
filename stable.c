/* File name: stable.c
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

#include "stable.h"

static void st_setsize(void);
static void st_incoffset(void); 
extern STD sym_table;

/*
Purpose: Looks up the given string in the symbol table's buffer
Author: Liam Wilkinson
History: 1.0
Called functions: strcmp, b_setmark, strlen
Parameters: valid symbol table and char pointer
Return value: int with the location of the lexeme in the buffer
Algorithm: iterates through all the strings in the buffer and compares them.
*/
int st_lookup(STD sym_table, char* lexeme){
	int i;
	short currentOffset = 0;
	char* head;
	if (sym_table.st_size <= 0){
		return -2;
	}
	head = b_setmark(sym_table.plsBD, 0);
	for (i = 0; i < sym_table.st_offset; i++){
		if (strcmp(lexeme, head + currentOffset) == 0){
			return i;
		}
		currentOffset += (short)(strlen(head + currentOffset) + 1);
	}
	return -1;
}

/*
Purpose: installs lexeme into the symbol table
Author: Liam Wilkinson
History: 1.0
Called functions: 
Parameters: valid symbol table, valid lexeme pointer, char type and current line
Return value: int with the location of the symbol in the symbol table
Algorithm: 
*/
int st_install(STD sym_table, char* lexeme, char type, int line){
	int offset;
	unsigned int i;
	char reallocated = 0;
	/* If the table failed in malloc then don't continue */
	if (sym_table.st_offset >= sym_table.st_size){
		return -1;
	}
	/*If lex is already in the table return the offset*/
	if ((offset = st_lookup(sym_table, lexeme)) > -1){
		return offset;
	}
	if (sym_table.st_size <= 0){
		return -2;
	}
	sym_table.pstvr[sym_table.st_offset].plex = b_setmark(sym_table.plsBD, b_size(sym_table.plsBD));
	sym_table.pstvr[sym_table.st_offset].o_line = line;
	/* add lexeme string to buffer */
	for (i = 0; i < strlen(lexeme); i++){
		if (!b_addc(sym_table.plsBD, lexeme[i])){
			return -2;
		}
		if (b_rflag(sym_table.plsBD) == SET_R_FLAG){
			reallocated = 1;
		}
	}
	if (!b_addc(sym_table.plsBD, '\0')){
			return -1;
	}
	sym_table.pstvr[sym_table.st_offset].o_line = line;
	/* Status field is set to zero and based on it's type it's set with one of the type masks */
	sym_table.pstvr[sym_table.st_offset].status_field = DEFAULT_MASK;
	switch(type){
		case 'I': sym_table.pstvr[sym_table.st_offset].status_field |= INT_MASK; sym_table.pstvr[sym_table.st_offset].i_value.int_val = 0; break;
		case 'F': sym_table.pstvr[sym_table.st_offset].status_field |= FLOAT_MASK; sym_table.pstvr[sym_table.st_offset].i_value.fpl_val = 0.0f; break;
		case 'S': sym_table.pstvr[sym_table.st_offset].status_field |= STRING_MASK; sym_table.pstvr[sym_table.st_offset].i_value.str_offset = -1; break;
		default: return -1;
	}
	if (reallocated){
		int i;
		int currentOffset = 0;
		sym_table.pstvr[0].plex = b_setmark(sym_table.plsBD, 0);
		for (i = 0; i < sym_table.st_offset; i++){
			sym_table.pstvr[i].plex = sym_table.pstvr[0].plex + currentOffset;
			currentOffset += (strlen(sym_table.pstvr[i].plex) + 1);
		}
	}
	/* increment the offset */
	st_incoffset();
	return sym_table.st_offset;
}

/*
Purpose: creates symbol table
Author: Liam Wilkinson
History: 1.0
Called functions: 
Parameters: valid symbol table size
Return value: int with the location of the lexeme in the buffer
Algorithm: N/A
*/
STD st_create(int st_size){
	STD new_table;
	if (st_size <= 0 || ((new_table.pstvr = (STVR*) malloc((size_t)st_size * sizeof(STVR))) == NULL)){
		new_table.st_size = 0;
	}
	if ((new_table.plsBD = b_create(st_size < 300? 300 : st_size, (short)1,'a')) == NULL){
		free(new_table.plsBD);
		new_table.st_size = 0;
	}
	new_table.st_size = st_size;
	new_table.st_offset = 0;
	return new_table;
}

/*
Purpose: updates the type system bits if it hasn't been updated already
Author: Liam Wilkinson
History: 1.0
Called functions: N/A
Parameters: valid symbol table, valid vid_offset integer and valid v_type (I, F or S)
Return value: int with the location of the lexeme in the buffer
Algorithm: N/A
*/
int st_update_type(STD sym_table, int vid_offset, char v_type){
	/* If update flag is set then fail */
	if ((sym_table.pstvr[vid_offset].status_field & UPDATE_MASK) || sym_table.st_size <= 0){
		return -2;
	}
	/* Resets type */
	sym_table.pstvr[vid_offset].status_field = sym_table.pstvr[vid_offset].status_field & DEFAULT_UPDATE_MASK;
	switch(v_type){
		case 'I': sym_table.pstvr[vid_offset].status_field |= INT_MASK; break;
		case 'F': sym_table.pstvr[vid_offset].status_field |= FLOAT_MASK; break;
		case 'S': sym_table.pstvr[vid_offset].status_field |= STRING_MASK; break;
		default: return -2;
	}
	sym_table.pstvr[vid_offset].status_field |= UPDATE_MASK;
	return 1;
}

/*
Purpose: updates the value of a a record in the symbol table
Author: Liam Wilkinson
History: 1.0
Called functions: N/A
Parameters: valid sym_table, valid offset and valid i_value
Return value: void
Algorithm: N/A
*/
int st_update_value(STD sym_table, int vid_offset, InitialValue i_value){
	if (sym_table.st_size <= 0){
		return -2;
	}
	sym_table.pstvr[vid_offset].i_value = i_value;
	return vid_offset;
}

/*
Purpose: gets the type saved in the status_field of a certain record
Author: Liam Wilkinson
History: 1.0
Called functions: N/A
Parameters: valid sym_table, valid offset
Return value: void
Algorithm: N/A
*/
char st_get_type(STD sym_table, int vid_offset){
	unsigned short mask;
	if(sym_table.st_size <= 0){
		return -1;
	}
	mask = sym_table.pstvr[vid_offset].status_field & 0x0006;
	switch(mask){
		case INT_MASK: return 'I'; break;
		case FLOAT_MASK: return 'F'; break;
		case STRING_MASK: return 'S'; break;
		default: return -1;
	}
}

/*
Purpose: frees dynamically allocated memory
Author: Liam Wilkinson
History: 1.0
Called functions: N/A
Parameters: valid sym_table with pstvr set to a valid address of dynamically allocated memory
Return value: void
Algorithm: N/A
*/
void st_destroy(STD sym_table){
	if (sym_table.pstvr != NULL || sym_table.st_size <= 0){
		free(sym_table.pstvr);
		sym_table.pstvr = 0;
	}
	b_destroy(sym_table.plsBD);
}

/*
Purpose: prints out the symbol table
Author: Liam Wilkinson
History: 1.0
Called functions: N/A
Parameters: valid sym_table with records in it
Return value: the offset of the symbol table
Algorithm: N/A
*/
int st_print(STD sym_table){
	int i;
	if (sym_table.st_size <= 0){
		return -1;
	}
	printf("\nSymbol Table");
	printf("\n____________\n\n");
	printf("Line Number Variable Identifier\n");
	for (i = 0; i < sym_table.st_offset; ++i){
		printf("%2d        %s\n", sym_table.pstvr[i].o_line, sym_table.pstvr[i].plex);
	}
	return sym_table.st_offset;
}

/*
Purpose: outputs the contents of the symbol table out to a file 
Author: Liam Wilkinson
History: 1.0
Called functions: fopen, fprintf and strlen
Parameters: valid sym_table with records in it
Return value: the offset of the symbol table
Algorithm: N/A
*/
int st_store(STD sym_table){
	FILE* output;
	int i;
	if ((output = fopen("$stable.ste", "w+")) == NULL){
		return -1;
	}
	fprintf(output, "%d", sym_table.st_size);
	for (i = 0; i < sym_table.st_size; i++){
		fprintf(output, " %4X", sym_table.pstvr[i].status_field);
		fprintf(output, " %d", (int) strlen(sym_table.pstvr[i].plex));
		fprintf(output, " %s", sym_table.pstvr[i].plex);
		fprintf(output, " %d", sym_table.pstvr[i].o_line);
		switch(st_get_type(sym_table, i)){
			case 'F': fprintf(output, " %.2f", sym_table.pstvr[i].i_value.fpl_val); break;
			case 'I': fprintf(output, " %d", sym_table.pstvr[i].i_value.int_val); break;
			case 'S': fprintf(output, " %d", sym_table.pstvr[i].i_value.str_offset); break;
		}
	}
	fclose(output);
	printf("\nSymbol Table stored.\n");
	return i;
}

/*
Purpose: to be the worlds best sorting algorithm, O(0) every time
Author: Liam Wilkinson
History: 1.0
Called functions: N/A
Parameters: input whatever you want so long as it matches the STD and char types
Return value: 0
Algorithm: this uses an advanced Null zuruck jedes Mal sorting algorithm theorized 
	in the early 60s by a team of former german logicians.
*/
int st_sort(STD sym_table, char order){
	/* Order and sym_table do nothing, need it to match the function headers */
	return 0;
}

/*
Purpose: sets st_offset in the external symbol table to zero
Author: Liam Wilkinson
History: 1.0
Called functions: N/A
Parameters: N/A
Return value: N/A
Algorithm: N/A
*/
void st_setsize(void){
	sym_table.st_offset = 0;
}

/*
Purpose: increments st_offset in the external symbol table to zero
Author: Liam Wilkinson
History: 1.0
Called functions: N/A
Parameters: N/A
Return value: N/A
Algorithm: N/A
*/
void st_incoffset(void){
	sym_table.st_offset++;
}