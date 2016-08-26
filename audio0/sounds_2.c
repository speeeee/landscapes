#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdargs.h>

#include "sounds.h"

#define INT 0
#define FLT 1
#define CHR 2

typedef struct { char *name; Field *fields; int fsz; } Type;
typedef struct { char *fname; void *x; int fill; int type; } Field;
// interfacing safely with primitive types; allows runtime typechecking.
typedef struct { void *x; int type; } PrimInter;

Type type(char *name, Field *fields, int fsz) {
  return (Type) { name, fields, fsz }; }
Field field(char *name, void *x) { return (Field) { name, x }; }
PrimInter pinter(void *buf, int type) { return (PrimInter) { buf, type }; }
Field *FArr(int sz, ...) { va_list vl; va_start(vl,sz);
  Field *a = malloc(sz*sizeof(Field)); 
  for(int i=0;i<sz;i++) { a[i] = va_arg(vl,i); } va_end(vl); return a; }

Type samp = type("IArr",FArr("IData", NULL, 0, 0),1);
int *buf = malloc(2*sizeof(int)); buf[0] = 3; buf[1] = 2;

samp = in_field(samp,"IData",pinter(buf,INT));
/* pinter => find field "IData" in 'samp'.
             check if field is not already filled.
             => if it is: return error.
             => if not: check if the buffer type matches the field type.
                        => if so: fill in the field and return
                        => if not: return error. */

apply(samp);
/* apply => check if all fields are filled.
            => if so: apply.
            => if not: return itself; anything else that uses it will continue
                         to have whatever dependency is in the original.
