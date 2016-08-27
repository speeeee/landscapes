#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

//#include "sounds.h"

#define INT 1
#define FLT 2
#define CHR 3

// organize this into files: freeing utilities, printing utilies, and main.

typedef void (*Freer)(void *x);
// 'typ' refers to a type name.
//typedef struct { char *typ; Freer x; } Fr;

typedef struct { void *x; Freer f; int prim; } Item;

typedef struct { char *fname; Item x; int fill; } Field;
typedef struct { char *name; Field *fields; int fsz; } Type;
// interfacing safely with primitive types; allows runtime typechecking.
typedef struct { char *name; Item x; } PrimInter;

typedef struct Stk { Item pt; int t; Freer typ; int *rc; struct Stk *prev; } Stk;

void print_field(Field); void print_type(Type);
void free_type(void *);

// for unfilled variables
void none(void *x) { return; }
Item wrap_int(int x) { int *a = malloc(sizeof(int));
  *a = x; return (Item) { (void *)a, none, 1 }; }
Item wrap_flt(float x) { float *a = malloc(sizeof(int));
  *a = x; return (Item) { (void *)a, none, 2 }; }
Item wrap_type(Type x) { Type *a = malloc(sizeof(Type));
  *a = x; return (Item) { (void *)a, free_type, 0 }; }

void print_int(int a) { printf("%i ", a); }
void print_float(float a) { printf("%f", a); }
void print_field(Field a) { printf("{%s ",a.fname);
  if(a.x.x) { switch(a.x.prim) { 
    case INT: print_int(*(int *)a.x.x); break;
    case FLT: print_float(*(float *)a.x.x); break;
    default: print_type(*(Type *)a.x.x); } printf("} "); }
  else { printf("_ "); } }
void print_type(Type a) { printf("(%s ",a.name);
  for(int i=0;i<a.fsz;i++) { print_field(a.fields[i]); } printf(") "); }

// types work somewhat like partial application.
Type type(char *name, Field *fields, int fsz) {
  return (Type) { name, fields, fsz }; }
Field field(char *name, Item x, int fill) { return (Field) { name, x, fill }; }
//PrimInter pinter(void *buf, int type) { return (PrimInter) { buf, type }; }
Field *fArr(int sz, ...) { va_list vl; va_start(vl,sz);
  Field *a = malloc(sz*sizeof(Field)); 
  for(int i=0;i<sz;i++) { a[i] = va_arg(vl,Field); } va_end(vl); return a; }

int *iArr(int sz, ...) { va_list vl; va_start(vl,sz);
  int *a = malloc(sz*sizeof(int)); for(int i=0;i<sz;i++) { a[i] = va_arg(vl,int); }
  va_end(vl); return a; }

void free_type(void *b) { Type a = *(Type *)b;
  for(int i=0;i<a.fsz;i++) { free(a.fields[i].fname);
    (a.fields[i].x.f)(a.fields[i].x.x); } free(a.name); }

// in_field
// a note is that in_field mutates the Type, but it shouldn't matter since
// the stack makes deep copies before using.
Type in_field(Type a, char *name, Item x) { int i;
  for(i=0;i<a.fsz;i++) { if(!strcmp(name,a.fields[i].fname)) {
    if(!a.fields[i].fill) { 
      a.fields[i] = (Field) { a.fields[i].fname, x, 1 }; }
    else { printf("ERROR: Data cannot be mutated after assignment.\n"); } } }
  if(i==a.fsz) { printf("ERROR: no such field `%s' in `%s'.\n", name, a.name); }
  return a; }

// upon calling a type like 'Sample', it will take the stack and return named fields.
// e.g. {1,2,3} 3 IntArr => (IntArr) { {1,2,3}, 3 }
//      3 IntArr => (Type) { "IntArr", { { "IData", NULL, 0 }, { "Size", 3, 1 } }, 2 } 

/*Type samp = type("IArr",FArr("IData", NULL, 0, 0),1);
//int *buf = malloc(2*sizeof(int)); buf[0] = 3; buf[1] = 2;

PrimInter *e = malloc(sizeof(PrimInter));
e = pinter("Int",buf,INT);
samp = in_field(samp,"IData",(void *)e);*/
/* pinter => find field "IData" in 'samp'.
             check if field is not already filled.
             => if it is: return error.
             => if not: check if the buffer type matches the field type.
                        => if so: fill in the field and return
                        => if not: return error. */

//apply(samp);
/* apply => check if all fields are filled.
            => if so: apply.
            => if not: return itself; anything else that uses it will continue
                         to have whatever dependency is in the original. */

int main(int argc, char **argv) {
  Type a = type("IArr", fArr(1,field("IData", wrap_int(3), 1)), 1);
  print_type(a); /*free_type((void *)&a);*/ return 0; }
