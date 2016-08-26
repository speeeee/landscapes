#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdargs.h>

#include "sounds.h"

#define INT 1
#define FLT 2
#define CHR 3

typedef void (*Freer)(void *x);
// 'typ' refers to a type name.
//typedef struct { char *typ; Freer x; } Fr;

typedef struct Item { void *x; Freer f; int prim };

typedef struct { char *name; Field *fields; int fsz; } Type;
typedef struct { char *fname; Item x; int fill; } Field;
// interfacing safely with primitive types; allows runtime typechecking.
typedef struct { char *name; Item x; int type; } PrimInter;

typedef struct Stk { Item pt; int t; FTy typ; int *rc; struct Stk *prev; } Stk;

// for unfilled variables
void none(void *x) { return; }

print_field(Field); print_type(Type);

void print_int(int a) { printf("%i ", a); }
void print_flt(float a) { printf("%f", a); }
void print_field(Field a) { printf("{%s ",a.fname);
  if(a.x.x) { switch(a.x.prim) { 
    case INT: print_int(*(int *)a.x.x); case FLT: print_float(*(float *)a.x.x);
    default: print_type(*(Type *)a.x.x); } printf("} "); }
  else { printf("_ "); }
void print_type(Type a) { printf("(%s ",a.name);
  for(int i=0;i<a.fsz;i++) { print_field(a.fields[i]); } printf(") "); }

// types work somewhat like partial application.
Type type(char *name, Field *fields, int fsz) {
  return (Type) { name, fields, fsz }; }
Field field(char *name, void *x) { return (Field) { name, x }; }
PrimInter pinter(void *buf, int type) { return (PrimInter) { buf, type }; }
Field *FArr(int sz, ...) { va_list vl; va_start(vl,sz);
  Field *a = malloc(sz*sizeof(Field)); 
  for(int i=0;i<sz;i++) { a[i] = va_arg(vl,Field); } va_end(vl); return a; }

void free_type(void *a) { Type b = *(Type *)a;
  for(int i=0;i<a.fsz;i++) { free(a.fields[i].name);
    (a.fields[i].x.f)(a.fields[i].x.x); } free(a.name); }

// in_field
// a note is that in_field mutates the Type, but it shouldn't matter since
// the stack makes deep copies before using.
Type in_field(Type a, char *name, void *x) { int i;
  for(i=0;i<a.fsz;i++) { if(!strcmp(name,a.fields[i].fname)) {
    if(!a.fields[i].fill) { 
      a.fields[i] = (Field) { a.fields[i].fname, x, 1 }; }
    else { printf("ERROR: Data cannot be mutated after assignment.\n"); } } }
  if(i==a.fsz) { printf("ERROR: no such field `%s' in `%s'.\n", name, a.name); }
  return a; }



// upon calling a type like 'Sample', it will take the stack and return named fields.
// e.g. {1,2,3} 3 IntArr => (IntArr) { {1,2,3}, 3 }
//      3 IntArr => (Type) { "IntArr", { { "IData", NULL, 0 }, { "Size", 3, 1 } }, 2 } 

Type samp = type("IArr",FArr("IData", NULL, 0, 0),1);
int *buf = malloc(2*sizeof(int)); buf[0] = 3; buf[1] = 2;

PrimInter *e = malloc(sizeof(PrimInter));
e = pinter("Int",buf,INT);
samp = in_field(samp,"IData",(void *)e);
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
