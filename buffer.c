/*
File name: buffer.c
Compiler: MS Visual Studio 2012
Author: Liam Wilkinson, 040771607
Course: CST 8152 - Compilers, Lab Section: 012
Assignment: 1
Date: 2014-01-29
Professor: Sv. Ramev
Purpose:
Function list: b_create(), b_addc(), b_isfull(), b_reset(), b_destroy(), b_size(), b_capacity(),
b_setmark(), b_mark(), b_mode(), b_inc_fact(), b_load(), b_isempty(), b_eob(), b_getc(),
b_print(), b_pack(), b_rflag(), b_retract(), b_retract_to_mark(), b_getc_offset()
*/

#include "buffer.h"

/*
Purpose: Builds the buffer based on given parameters
Author: Liam Wilkinson
History: 1.1
Called functions: calloc and malloc
Parameters: init_capacity short must be positive and within short range, inc_factor must be postive and
within inc_factor range and be in range of the o_mode, char o_mode must be a 0 or 1 and match with the inc_factor
Return value: pointer to created buffer
Algorithm: Sets everything to what it should be
*/
Buffer* b_create(short init_capacity, char inc_factor, char o_mode){
	Buffer* newBuffer;
	if (init_capacity < 0 || (SHRT_MAX < (unsigned)init_capacity)){
		return NULL;
	}
	/* C will short circuit if buffer is a null pointer so and return fail conditions
	before newBuffer is dereferenced as a null pointer and segfault. */
	if (1){
		if ((newBuffer = (Buffer*)calloc(init_capacity, sizeof(Buffer))) == NULL){
			return NULL;
		}
		if ((newBuffer->ca_head = (char*)malloc(init_capacity)) == NULL){
			free(newBuffer);
			return NULL;
		}
	}
	if ((o_mode == 'f') || (inc_factor == 0)){
		newBuffer->mode = 0;
	}
	else if (o_mode == 'a' && (inc_factor >= 1 && (unsigned char)inc_factor <= 255)) {
		newBuffer->mode = 1;
		newBuffer->inc_factor = (unsigned char)inc_factor;
	}
	else if (o_mode == 'm' && (inc_factor >= 1 && inc_factor <= 100)) {
		newBuffer->mode = -1;
		newBuffer->inc_factor = inc_factor;
	}
	else {
		return NULL;
	}
	newBuffer->capacity = init_capacity;
	return newBuffer;
}

/*
Purpose: Adds char to buffer if not full and increments the addc_offset
Author: Liam Wilkinson
History: 1.2
Called functions: b_isfull()
Parameters: valid pBuffer and char to be added to the buffer
Return value: the pBuffer given to the function with the additions made to it
Algorithm: N/A
*/
pBuffer b_addc(pBuffer const pBD, char symbol){
	/* availble_space, new_increment and new_capacity are all for calculating the capacity. All of them are ints
	to make sure that something greater than SHRT_MAX is being put into a short */
	int availble_space = 0, new_capacity = 0, new_increment = 0;
	/* old_pointer is for comparing the old pointer to the new pointer */
	char* old_pointer;
	if (pBD == NULL || pBD->addc_offset == SHRT_MAX){
		return pBD;
	}
	pBD->r_flag = 0;
	if (!b_isfull(pBD)){
		*(pBD->ca_head + pBD->addc_offset) = symbol;
		(pBD->addc_offset)++;
		return pBD;
	}
	else if (pBD->mode == 0) {
		return pBD;
	} else {
		availble_space = SHRT_MAX - pBD->capacity;
		new_increment = (availble_space * (unsigned int)pBD->inc_factor) / 100;
		new_capacity = pBD->capacity + new_increment;
		if (pBD->mode == -1){
			new_capacity = pBD->capacity + 1;
		}
		if ((unsigned)new_capacity > SHRT_MAX){
			new_capacity = SHRT_MAX;
		}
		old_pointer = pBD->ca_head;
		if ((old_pointer = (char*)realloc(old_pointer, new_capacity)) == NULL){
			return pBD;
		}
		if (old_pointer != pBD->ca_head){
			pBD->r_flag = SET_R_FLAG;
		}
		*(old_pointer + pBD->addc_offset) = symbol;
		pBD->ca_head = old_pointer;
		pBD->capacity = (short)new_capacity;
		pBD->addc_offset++;
		return pBD;
	}
}

/*
Purpose: Checks if the buffer is full
Author: Liam Wilkinson
History: 1.1
Called functions: b_size, b_capacity
Parameters: A valid Buffer pointer
Return value: negative int if there is an error, 1 (True) if the b_capacity is bigger than the b_size or a 0 if the opposite is true
Algorithm: N/A
*/
/* NEVER CALL THIS MACRO IF YOU AREN'T SURE THAT pBD ISN'T A VALID POINTER!*/
#ifdef B_FULL
#define (b_size(pBD) == (b_capacity(pBD) * sizeof(char))
#else
int b_isfull(Buffer* const pBD){
	if (pBD == NULL){
		return R_FAIL_1;
	}
	return (unsigned) b_size(pBD) == (b_capacity(pBD) * sizeof(char));
}
#endif

/*
Purpose: Resets the location property to the start of the buffer
Author: Liam Wilkinson
History: 1.1
Called functions: N/A
Parameters: valid pointer to a buffer
Return value: int that represents if the function failed or not
Algorithm: N/A
*/
int b_reset(Buffer *const pBD){
	if (pBD == 0){
		return 0;
	}
	pBD->eob = 0;
	pBD->getc_offset = 0;
	pBD->mark_offset = 0;
	pBD->r_flag = 0;
	pBD->addc_offset = 0;
	return 1;
}

/*
Purpose: Frees the buffer dynamically allocated memory
Author: Liam Wilkinson
History: 1.1
Called functions: free
Parameters: A valid Buffer pointer that has memory allocated to it
Return value: void
Algorithm: if allocated memory is allocated free them.
*/
void b_destroy(Buffer* const pBD){
	if (pBD == NULL){
		return;
	}
	if (pBD->ca_head != NULL){
		free(pBD->ca_head);
		pBD->ca_head = NULL;
	}
	if (pBD != NULL){
		free(pBD);
	}
}

/*
Purpose: returns the size of the buffer
Author: Liam Wilkinson
History: 1.1
Called functions: N/A
Parameters:valid pointer to a buffer with an allocated array of chars in ca_head.
Return value: returns the amount of chars actually used in the buffer
Algorithm:
*/
short b_size(Buffer* const pBD){
	if (pBD == NULL){
		return 0;
	}
	return pBD->addc_offset;
}

/*
Purpose: Returns the capacity of the given pointer buffer
Author: Liam Wilkinson
History: 1.1
Called functions: N/A
Parameters: valid pointer to a buffer
Return value: returns capacity from given buffer pointer
Algorithm:
*/
short b_capacity(Buffer* const pBD){
	if (pBD == NULL){
		return 0;
	}
	return pBD->capacity;
}

/*
Purpose: sets make_offset to given mark and returns
Author: Liam Wilkinson
History: 1.1
Called functions: b_capacity
Parameters: short mark between inclusive 0 and b_capacity
Return value: char* to the current char that mark_offset is at
Algorithm: ca_head is being multiplied, derefrenced, refrenced and sent out
*/
char* b_setmark(Buffer* const pBD, short mark){
	if (pBD != NULL || mark <= (unsigned short)b_capacity(pBD) || mark >= 0){
		pBD->mark_offset = mark;
		return &pBD->ca_head[pBD->mark_offset];
	}
	else {
		return NULL;
	}
}

/*
Purpose: returns the mark offset of a given buffer pointer
Author: Liam Wilkinson
History: 1.1
Called functions: N/A
Parameters: valid pointer to a buffer
Return value: returns short with the value of the member mark_offset
Algorithm: N/A
*/
short b_mark(Buffer* const pBD){
	if (pBD == NULL){
		return 0;
	}
	return pBD->mark_offset;
}

/*
Purpose: Returns the mode of the given buffer
Author: Liam Wilkinson
History: 1.1
Called functions: N/A
Parameters: valid pointer to a buffer
Return value: an int with the size of the buffer
Algorithm: N/A
*/
int b_mode(Buffer* const pBD){
	if (pBD == NULL){
		return 0;
	}
	return pBD->mode;
}

/*
Purpose: returns the inc_factor of the buffer or 256 if the inc_factor is negative
Author: Liam Wilkinson
History: 1.1
Called functions: N/A
Parameters: a valid pointer to a valid Buffer stuct
Return value: size_t containing the value in inc_factor of the given buffer pointer
Algorithm: N/A
*/
size_t b_inc_factor(Buffer* const pBD){
	if (pBD == NULL){
		return (size_t)0;
	}
	if ((unsigned)pBD->inc_factor > 0){
		return (size_t)pBD->inc_factor;
	}
	else {
		return (size_t)256;
	}
}

/*
Purpose: Puts file contents into buffer
Author: Liam Wilkinson
History: 1.1
Called functions: feof(), b_addc()
Parameters: FILE* fi needs to point to a file struct that exists and is currently open. Buffer* pBD needs to point to a valid Buffer
Return value: int, load fail errors return R_FAIL_2 (-2), other errors are -1, positive integers are the amount of added chars
Algorithm: Loops file, adds all the chars into the buffer. If it fails to be added to the buffer null is returned else it returns the amount of chars added.
*/
int b_load(FILE* const fi, Buffer* const pBD){
	char temp_char;
	/* added_chars keeps track of the amount of chars that have been added to the buffer */
	int added_chars = 0;
	if (pBD == NULL || fi == NULL){
		return 0;
	}
	while ((temp_char = (char)fgetc(fi)) != EOF){
		if (b_addc(pBD, temp_char) == NULL){
			return LOAD_FAIL;
		}
		added_chars++;
	}
	return added_chars;
}

/*
Purpose: Checks if the buffer is empty
Author: Liam Wilkinson
History: 1.1
Called functions: N/A
Parameters: Buffer* pBD needs to point to a valid Buffer
Return value: PASS or FAIL
Algorithm:
*/
int b_isempty(Buffer* const pBD){
	if (pBD == NULL){
		return R_FAIL_1;
	}
	return pBD->addc_offset <= 0;
}

/*
Purpose: Returns eob from the struct
Author: Liam Wilkinson
History: 1.1
Called functions: N/A
Parameters: Buffer* pBD needs to point to a valid Buffer
Return value: the integer memeber eob from the Buffer struct pointer sent in
Algorithm: N/A
*/
int b_eob(Buffer* const pBD){
	if (pBD == NULL){
		return R_FAIL_1;
	}
	return pBD->eob;
}

/*
Purpose: returns a char from the buffer and increments the getc_offset
Author: Liam Wilkinson
History: 1.1
Called functions: N/A
Parameters: Buffer* pBD needs to point to a valid Buffer
Return value: an int R_FAIL_2 or R_FAIL_1 if the function fails or the offset before it's incremented
Algorithm:
*/
char b_getc(Buffer* const pBD){
	short tempc_offset;
	if (pBD == NULL){
		return R_FAIL_2;
	}
	else if (pBD->getc_offset >= pBD->addc_offset){
		pBD->eob = 1;
		return R_FAIL_1;
	}
	else {
		pBD->eob = 0;
		tempc_offset = pBD->getc_offset;
		pBD->getc_offset++;
		return pBD->ca_head[tempc_offset];
	}
}

/*
Purpose: Prints out the chars in the buffer
Author: Liam Wilkinson
History: 1.1
Called functions: b_isempty, b_eob, b_getc
Parameters: Buffer* pBD needs to point to a valid Buffer
Return value: an int to check if it failed or not
Algorithm: if the buffer is empty return 1 and say that it's empty. Else loop through, get each char and print them
*/
int b_print(Buffer* const pBD){
	char temp;
	if (pBD == NULL || pBD->ca_head == NULL){
		return R_FAIL_1;
	}
	if (b_isempty(pBD)){
		printf("The buffer is empty.\n");
	}
	else {
		pBD->getc_offset = 0;
		while (!b_eob(pBD)){
			if((temp= b_getc(pBD))!='ÿ'){
				printf("%c", temp);
			}
		}
		pBD->getc_offset = 0;
		printf("\n");
	}
	return R_FAIL_1;
}

/*
Purpose: Increases the size of the buffer, finally fixed version
Author: Liam Wilkinson
History: 3.0
Called functions: realloc
Parameters: Buffer* pBD needs to point to a valid Buffer
Return value: pointer to the given buffer or NULL if it fails
Algorithm: N/A
*/
Buffer* b_pack(Buffer* const pBD){
	char* temp;
	if (pBD->addc_offset == SHRT_MAX || pBD->addc_offset > pBD->capacity || pBD->addc_offset < 0){
		return NULL;
	}
	if ((temp = (char*) realloc(pBD->ca_head, pBD->addc_offset + 1)) == NULL){
		return NULL;
	}
	if (temp!= pBD->ca_head){
		pBD->r_flag = 1;
		pBD->ca_head = temp;
	}
	pBD->capacity = (pBD->addc_offset + 1);
	return pBD;
}

/*
Purpose: provides access to the r_flag member
Author: Liam Wilkinson
History: 1.1
Called functions: N/A
Parameters: Buffer* pBD needs to point to a valid Buffer
Return value: r_flag
Algorithm: N/A
*/
char b_rflag(Buffer* const pBD){
	return !pBD ? (char)R_FAIL_1 : pBD->r_flag;
}

/*
Purpose:retracts getc_offset
Author: Liam Wilkinson
History: 1.1
Called functions: N/A
Parameters: Buffer* pBD needs to point to a valid Buffer
Return value: getc_offset short
Algorithm: N/A
*/
short b_retract(Buffer* const pBD){
	if (pBD->getc_offset > 0 && pBD != NULL){
		(pBD->getc_offset)--;
		return pBD->getc_offset;
	}
	else {
		return R_FAIL_1;
	}
}

/*
Purpose: retracts getc_offset to mark_offset and returns getc_offset
Author: Liam Wilkinson
History: 1.1
Called functions: N/A
Parameters: Buffer* pBD needs to point to a valid Buffer
Return value: short getc_offset
Algorithm: N/A
*/
short b_retract_to_mark(Buffer* const pBD){
	if (pBD == NULL){
		return R_FAIL_1;
	}
	pBD->getc_offset = pBD->mark_offset;
	return pBD->getc_offset;
}

/*
Purpose: Returns getc_offset from Buffer struct pointer
Author: Liam Wilkinson
History: 1.1
Called functions: N/A
Parameters: Buffer* pBD needs to point to a valid Buffer
Return value: short int from the member getc_offset in the given Buffer structure pointer
Algorithm: N/A
*/
short b_getc_offset(Buffer* const pBD){
	return !pBD ? R_FAIL_1 : pBD->getc_offset;
}