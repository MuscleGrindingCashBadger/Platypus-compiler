/* REPLACE the header below with your file header (see CST8152_ASSAMG.pdf for details).
 * File Name: buffer.h
 * Version: 1.15.1
 * Author: S^R
 * Date: 5 January 2015
 * Preprocessor directives, type declarations and prototypes necessary for buffer implementation 
 * as required for CST8152, Assignment #1, Winter 2015.
 * The file is not completed.
 * You must add your function declarations (prototypes).
 * You must also add your constant definitions and macros,if any.
 */
#ifndef BUFFER_H_
#define BUFFER_H_

/*#pragma warning(1:4001) *//*to enforce C89 type comments  - to make //comments an warning */

/*#pragma warning(error:4001)*//* to enforce C89 comments - to make // comments an error */

/* standard header files */
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* constant definitions */
/* You may add your own constant definitions here */
#define R_FAIL_1 -1         /* fail return value */
#define R_FAIL_2 -2         /* fail return value */
#define LOAD_FAIL -2       /* load fail error */
#define SET_R_FLAG 1       /* realloc flag set value */


/* user data type declarations */
typedef struct BufferDescriptor {
    char *ca_head;   /* pointer to the beginning of character array (character buffer) */
    short capacity;    /* current dynamic memory size (in bytes) allocated to character buffer */
    short addc_offset;  /* the offset (in chars) to the add-character location */
    short getc_offset;  /* the offset (in chars) to the get-character location */
    short mark_offset; /* the offset (in chars) to the mark location */
    char  inc_factor; /* character array increment factor */
    char  r_flag;     /* character array reallocation flag */
    char  mode;       /* operational mode indicator*/
    int   eob;       /* end-of-buffer reached flag */
} Buffer, *pBuffer;
/*typedef Buffer *pBuffer;*/

/* function declarations */
Buffer* b_create(short init_capacity, char inc_factor, char o_mode);
pBuffer b_addc(pBuffer const pBD, char symbol); 
int b_isfull(Buffer* const pBD); 
int b_reset(Buffer *const pBD);
void b_destroy(Buffer* const pBD);
short b_size(Buffer* const pBD);
short b_capacity(Buffer* const pBD);
char* b_setmark(Buffer* const pBD, short mark);
short b_mark(Buffer* const pBD);
int b_mode(Buffer* const pBD);
size_t b_inc_factor(Buffer* const pBD);
int b_load(FILE* const fi, Buffer* const pBD);
int b_isempty(Buffer* const pBD);
int b_eob(Buffer* const pBD); 
char b_getc(Buffer* const pBD); 
int b_print(Buffer* const pBD); 
Buffer* b_pack(Buffer* const pBD);
char b_rflag(Buffer* const pBD);
short b_retract(Buffer* const pBD);
short b_retract_to_mark(Buffer* const pBD);
short b_getc_offset(Buffer* const pBD);

#endif
