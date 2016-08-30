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
typedef void *(*Copier)(void *);

// 'typ' refers to a type name.
//typedef struct { char *typ; Freer x; } Fr;

typedef struct { void *x; Freer f; Copier c; const char *type; } Item;

typedef struct TypeSig TypeSig;
typedef struct { const char *tname; } T;
typedef struct TypeSig { T t; struct TypeSig *next; } TypeSig;

typedef struct { const char *fname; Item x; } Field;
typedef struct { const char *name; Field *fields; int fsz; } Type;
// interfacing safely with primitive types; allows runtime typechecking.
typedef struct { const char *name; Item x; } PrimInter;

typedef struct Stk { Item pt; /*Freer typ;*/ int *rc; struct Stk *prev; } Stk;

typedef Item (*TFun)(Type);
typedef Stk *(*SFun)(Stk *);

void print_field(Field); void print_type(Type);

void free_type(void *);
void free_stk(void *);
void free_queue(void *);

Item item(void *, Freer, Copier, const char *);

void *copy_item(void *);
void *copy_field(void *);
void *copy_type(void *);

void *copy_queue(void *);
void *copy_stk(void *);

// for unfilled variables
void none(void *x) { return; }
void *id(void *x) { return x; }
void *copy_int(void *x) { int *i = malloc(sizeof(int));
  *i = *(int *)x; return (void *)i; }

// == Queue management ======================= //

// to-be-fixed
TypeSig *to_queue(TypeSig *a, T t) /*{ TypeSig *e = malloc(sizeof(TypeSig));
  e->t = t; e->next = NULL; if(a) { a->next = e; return a; } return e; }*/
{ TypeSig *a2 = a;
   TypeSig *e = malloc(sizeof(TypeSig)); e->t = t; e->next = NULL;
   if(a2) { while(a2->next) { a2 = a2->next; } a2->next = e; }
   else { a = e; } return a; }
TypeSig *pass_queue(TypeSig *a) { TypeSig *e = a; a = a->next;
  free(e); return a; }

// == TypeSig ================================ //

T typ(const char *n) { return (T) { n }; }
TypeSig *type_sig(int sz, ...) { va_list vl; va_start(vl,sz); TypeSig *a = NULL;
  for(int i=0;i<sz;i++) { a = to_queue(a,va_arg(vl,T)); } return a; }

// == Stack operations ======================= //

Stk *push(Item y,int *rc,Stk *x) { Stk *e = malloc(sizeof(Stk));
  e->pt = y; e->rc = rc; e->prev = NULL;
  if(x) { e->prev = x; } x = e; return x; }
Stk *pushr(Item y,Stk *x) { int *rc = malloc(sizeof(int)); *rc = 1;
  return push(y,rc,x); }
Stk *pop(Stk *x) { Stk *q = x->prev;
  if(--(*(x->rc))<1) { (x->pt.f)(x->pt.x); 
                       free(x->rc); } 
  x = q; return x; }

// WARNING: using this will cause memory leaks unless the item is pushed back.
//        : for use only with destructive operations (such as curry).
Stk *pop_shallow(Stk *x) { return x->prev; }

Stk *stk_append(Stk *a, Stk *b) { if(b) { a = stk_append(a,b->prev);
  // case for copy_item_no_rt.
  Item *e = (Item *)copy_item((void *)&b->pt);
  a = pushr(*e,a); free(e); free(b); } return a; }

// == Stack pushes =========================== //

Stk *push_int(int y, Stk *x) { int *e = malloc(sizeof(int));
  *e = y; Item i = item((void *)e,free,copy_int,"Int"); return pushr(i,x); }
Stk *push_type(Type y, Stk *x) { Type *e = malloc(sizeof(Type));
  *e = y; Item i = item((void *)e,free_type,copy_type,"Type"); return pushr(i,x); }
// used only by 'curry' to make it less tedious.
Stk *push_type_shallow(Type y, Stk *x) { Type *e = malloc(sizeof(Type));
  *e = y; Item i = item((void *)e,none,id,"Type"); return pushr(i,x); }

// == Printing =============================== //

/*void print_int(int a) { printf("%i ", a); }
void print_float(float a) { printf("%f", a); }
void print_field(Field a) { printf("{%s ",a.fname);
  if(a.x.fill) { switch(a.x.prim) { 
    case INT: print_int(*(int *)a.x.x); break;
    case FLT: print_float(*(float *)a.x.x); break;
    default: print_type(*(Type *)a.x.x); } printf("} "); }
  else { printf("_ "); } }
void print_type(Type a) { printf("(%s ",a.name);
  for(int i=0;i<a.fsz;i++) { print_field(a.fields[i]); } printf(") "); }*/

// == Construction =========================== //

Item item(void *x, Freer f, Copier c, const char *n) { return (Item) { x, f, c, n }; }

// types work somewhat like partial application.
Type type(char *name, Field *fields, int fsz) {
  return (Type) { name, fields, fsz }; }
Field field(const char *name, Item x) { return (Field) { name, x }; }
//PrimInter pinter(void *buf, int type) { return (PrimInter) { buf, type }; }
Field *fArr(int sz, ...) { va_list vl; va_start(vl,sz);
  Field *a = malloc(sz*sizeof(Field)); 
  for(int i=0;i<sz;i++) { a[i] = va_arg(vl,Field); } va_end(vl); return a; }

int *iArr(int sz, ...) { va_list vl; va_start(vl,sz);
  int *a = malloc(sz*sizeof(int)); for(int i=0;i<sz;i++) { a[i] = va_arg(vl,int); }
  va_end(vl); return a; }

// == Freedom ================================= //

void free_type(void *b) { Type a = *(Type *)b;
  for(int i=0;i<a.fsz;i++) { //free(a.fields[i].fname);
    (a.fields[i].x.f)(a.fields[i].x.x); } free(a.fields); a.fields = NULL; }
void free_stk(void *a) { Stk *x = (Stk *)a; while(x) { x = pop(x); } }
void free_queue(void *a) { TypeSig *b = (TypeSig *)a;
  if(a) { free(a); if(b) { free_queue((void *)b->next); } } }

// == Copying ================================= //

// make no_rt for all copiers.

void *copy_item(void *a) { Item b = *(Item *)a;
  Item *c = malloc(sizeof(Item)); *c = item((b.c)(b.x), b.f, b.c, b.type);
  return (void *)c; }
void *copy_field(void *a) { Field b = *(Field *)a;
  // case for copy_item_no_rt.
  Item *f = (Item *)copy_item((void *)&b.x); // encased void pointer must be freed
                                             // here due to lack of context.
  Field *c = malloc(sizeof(Field)); *c = field(b.fname,*f); free(f);
  return (void *)c; }
Type copy_type_no_rt(Type b) { Type c;
  c.fsz = b.fsz; c.fields = malloc(c.fsz*sizeof(Field));
  // case for copy_field_no_rt.
  for(int i=0;i<c.fsz;i++) { Field *f = (Field *)copy_field((void *)&b.fields[i]);
    c.fields[i] = *f; free(f); } return c; }
void *copy_type(void *a) { Type b = *(Type *)a; Type *c = malloc(sizeof(Type));
  *c = copy_type_no_rt(b); return (void *)c; }

void *copy_queue(void *a) { TypeSig *b = (TypeSig *)a; TypeSig *c = NULL;
  while(b) { c = to_queue(c,b->t); b = b->next; } return (void *)c; }
void *copy_stk(void *a) { Stk *b = (Stk *)a; Stk *c = NULL;
  c = stk_append(c,b); return (void *)c; }

// == Field insertion ========================= //

// in_field
// a note is that in_field mutates the Type, but it shouldn't matter since
// the stack makes deep copies before using.
/*Type in_field(Type a, char *name, Item x) { int i;
  for(i=0;i<a.fsz;i++) { if(!strcmp(name,a.fields[i].fname)) {
    if(!a.fields[i].fill) { 
      a.fields[i] = (Field) { a.fields[i].fname, x, 1 }; }
    else { printf("ERROR: Data cannot be mutated after assignment.\n"); } } }
  if(i==a.fsz) { printf("ERROR: no such field `%s' in `%s'.\n", name, a.name); }
  return a; }*/

// (Fun {In (Item:char **)} {Out (Item:char **)} {Data (Item:Stk *)})

/* (Map {Fun (Inc {Int _})} {Arr (IArr {IData [1 2 3 4]} {Size 4})}) */

//apply(samp);
/* apply => check if all fields are filled.
            => if so: apply.
            => if not: return itself; anything else that uses it will continue
                         to have whatever dependency is in the original. */

// == Currying & others ======================== //

Stk *curry_slow(Stk *stk) { Type a = *(Type *)copy_type(stk->pt.x); 
  Item e = *(Item *)copy_item((void *)&stk->prev->pt);
  a.fields[2].x.x = pushr(e,a.fields[2].x.x);
  a.fields[0].x.x = pass_queue(a.fields[0].x.x);
  return push_type(a,pop(pop(stk))); }
// this is specifically destructive.  type reference MUST be copied before use.
Stk *curry(Stk *stk) { Type a = *(Type *)stk->pt.x;
  // case for copy_item_no_rt
  Item *e = (Item *)copy_item((void *)&stk->prev->pt);
  a.fields[2].x.x = pushr(*e,a.fields[2].x.x);
  a.fields[0].x.x = pass_queue(a.fields[0].x.x);
  return push_type(a,pop(pop_shallow(stk))); }
Stk *apply(Stk *stk) { Type a = *(Type *)stk->pt.x;
  if(a.fields[0].x.x) { return apply(curry(stk)); }
  else { a.fields[2].x.x = (*(SFun)a.fields[1].x.x)(a.fields[2].x.x);
         stk = stk_append(pop_shallow(stk),a.fields[2].x.x);
         free_type((void *)&a);
         return stk; } }

// == Sample function ========================== //

Stk *add(Stk *x) { int a = *(int *)x->pt.x; int b = *(int *)x->prev->pt.x;
  return push_int(a+b,pop(pop(x))); }

int main(int argc, char **argv) { Stk *stk;
  Type t_add = type("Fun"
    ,fArr(3,field("Type",item((void *)type_sig(2,typ("Int"),typ("Int"))
                             ,free_queue,copy_queue,"TypeSig"))
           ,field("F",item((void *)add,none,id,"TFun"))
           ,field("Data",item(NULL,free_stk,copy_stk,"Stack"))),3);
  stk = push_int(3,stk); stk = push_int(4,stk);
  stk = push_type(copy_type_no_rt(t_add),stk);
  stk = curry(stk); stk = curry(stk); stk = apply(stk);
  printf("%i\n",*(int *)stk->pt.x);
  return 0; }

// remember that 'stk_free' does not fully work considering 'push_type_shallow'.

/* evaluation of any kind on fields (including type-checking and 'apply'-ing) are
     done by the 'apply' function. */
