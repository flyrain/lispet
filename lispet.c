#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "mpc.h"
#include <math.h>

#define LASSERT(args, cond, err) if (!(cond)) {lval_del(args); return lval_err(err);}

#ifdef _WIN32

#include <string.h>

static char buffer[2048];

char * readline(char * prompt){
    fputs("lispy> ", stdout);
    fgets(buffer, 2048, stdin);
    char* cpy = malloc(strlen(buffer) + 1);
    strcpy(cpy, buffer);
    cpy[strlen(cpy - 1)] = '\0';
    return cpy;
}

void add_history(char * unused){}

#else //for Linux

#include <editline/readline.h>
#include <editline/history.h>

#endif

typedef struct lval{
    int type;
    
    long num;

    //error and symbol types have some string data
    char * err;
    char * sym;
    
    int count;
    struct lval ** cell;
}lval;

//Possible Lval Types
enum {LVAL_NUM, LVAL_ERR, LVAL_SYM, LVAL_SEXPR, LVAL_QEXPR};

//Possible Error Types
enum {LERR_DIV_ZERO, LERR_BAD_OP, LERR_BAD_NUM};

static lval* lval_num(long x)
{
    lval* v = malloc(sizeof(lval));
    v->type = LVAL_NUM;
    v->num = x;
    return v;
}

static lval* lval_err(char * m)
{
    lval* v = malloc(sizeof(lval));
    v->type = LVAL_ERR;
    v->err = malloc(strlen(m) + 1);
    strcpy(v->err, m);
    return v;
}

static lval* lval_sym(char * s)
{
    lval* v = malloc(sizeof(lval));
    v->type = LVAL_SYM;
    v->sym = malloc(strlen(s) + 1);
    strcpy(v->sym, s);
    return v;
}

static lval* lval_sexpr(void)
{
    lval* v = malloc(sizeof(lval));
    v->type = LVAL_SEXPR;
    v->count = 0; 
    v->cell = NULL;
    return v;
}

static lval* lval_qexpr(void)
{
    lval* v = malloc(sizeof(lval));
    v->type = LVAL_QEXPR;
    v->count = 0; 
    v->cell = NULL;
    return v;
}


static void lval_del(lval* v)
{
    switch (v->type) {
    case LVAL_NUM: break;
    case LVAL_ERR: free(v->err); break;
    case LVAL_SYM: free(v->sym); break;

    // if Qexpr and Sexpr then delete all elements inside
    case LVAL_QEXPR:
    case LVAL_SEXPR:     
        for(int i = 0; i < v->count; i ++)
            lval_del(v->cell[i]);
        
        free(v->cell);
        break;
    }

    free(v);
}

static lval* lval_add(lval* v, lval* x)
{
    v->count++;
    v->cell = realloc(v->cell, sizeof(lval*) * v->count);
    v->cell[v->count - 1] = x;
    return v;
}

static lval* lval_add_front(lval* v, lval* x)
{
    v->count++;
    v->cell = realloc(v->cell, sizeof(lval*) * v->count);
    memmove(&v->cell[1], &v->cell[0], sizeof(lval*) * (v->count - 1));
    v->cell[0] = x;
    return v;
}

static lval* lval_read_num(mpc_ast_t* t)
{
    long x = strtol(t->contents, NULL, 10);
    return errno != ERANGE ? lval_num(x) : lval_err("invalid number"); 
}

static lval* lval_read(mpc_ast_t* t)
{
    if(strstr(t->tag, "number")) { return lval_read_num(t);}
    if(strstr(t->tag, "symbol")) { return lval_sym(t->contents);}
    
    lval* x = NULL;
    // why? '>' is the tag of start point of ast
    if (strcmp(t->tag, ">") == 0) { x = lval_sexpr();} 
    if (strstr(t->tag, "sexpr")) { x = lval_sexpr();}
    if (strstr(t->tag, "qexpr")) { x = lval_qexpr();}
    
    for(int i = 0; i < t->children_num; i++) {
        assert(x != NULL);
        if (strcmp(t->children[i]->contents, "(") == 0) { continue; }
        if (strcmp(t->children[i]->contents, ")") == 0) { continue; }
        if (strcmp(t->children[i]->contents, "}") == 0) { continue; }
        if (strcmp(t->children[i]->contents, "{") == 0) { continue; }
        if (strcmp(t->children[i]->tag, "regex") == 0) { continue; }
        x = lval_add(x, lval_read(t->children[i]));
    }
    
    return x;
}

static void lval_print(lval* v);

static void lval_expr_print(lval* v, char open, char close)
{
    putchar(open);
    for (int i = 0; i < v->count; i++) {
        lval_print(v->cell[i]);

        if(i != (v->count - 1)){
            putchar(' ');
        }
    }
    putchar(close);
}

static void lval_print(lval* v)
{
    switch(v->type){
    case LVAL_NUM: printf("%li", v->num); break;
    case LVAL_ERR: printf("Error %s", v->err); break; 
    case LVAL_SYM: printf("%s", v->sym); break; 
    case LVAL_SEXPR: lval_expr_print(v, '(', ')'); break; 
    case LVAL_QEXPR: lval_expr_print(v, '{', '}'); break; 
    }
}

static void lval_println(lval* v) { lval_print(v); putchar('\n');}

static lval* lval_eval(lval* v);
static lval* lval_take(lval* v, int i);
static lval* lval_pop(lval* v, int i);
static lval* builtin_op(lval* a, char* op);
static lval* builtin_head(lval* a);
static lval* builtin_tail(lval* a);
static lval* builtin_list(lval* a);
static lval* builtin_eval(lval* a);
static lval* builtin_join(lval* a);
static lval* builtin_init(lval* a);
static lval* builtin_len(lval* a);
static lval* builtin_cons(lval* a);
static lval* builtin(lval* a, char* func);

static lval* lval_eval_sexpr(lval* v) 
{
    //evaluation children
    for (int i = 0; i < v->count; i++) {
        v->cell[i] = lval_eval(v->cell[i]); 
    }

    //error checking
    for (int i = 0; i < v->count; i++) {
        if(v->cell[i]->type == LVAL_ERR) { return lval_take(v,i);} 
    }

    //empty expression
    if(v->count == 0) { return v;}

    //single expression
    if(v->count == 1){ return lval_take(v, 0);}

    //ensure first element is symbol
    lval* f = lval_pop(v, 0);
    if(f->type != LVAL_SYM){
        lval_del(f); lval_del(v);
        return lval_err("S-expression Does not start with symbol!");
    }
    
    //call builtin with operator
    lval* result = builtin(v, f->sym);
    lval_del(f);
    return result;
}

static lval* lval_eval(lval* v)
{
    if(v->type == LVAL_SEXPR) { return lval_eval_sexpr(v);}
    return v;
}

static lval* lval_pop(lval* v, int i)
{
    lval* x = v->cell[i];
    
    //Shift the memory following the item at "i" over the top of it
    memmove(&v->cell[i], &v->cell[i+1], sizeof(lval*) * (v->count-i-1));
    
    v->count--;

    //reallocate to shrink the memory used 
    v->cell = realloc(v->cell, sizeof(lval*) * v->count);

    return x;
}

static lval* lval_take(lval* v, int i )
{
    lval* x = lval_pop(v,i);
    lval_del(v);
    return x;
}

static lval* builtin(lval* a, char* func)
{
    if (strcmp("head", func) == 0 ) { return builtin_head(a);}
    if (strcmp("tail", func) == 0 ) { return builtin_tail(a);}
    if (strcmp("list", func) == 0 ) { return builtin_list(a);}
    if (strcmp("eval", func) == 0 ) { return builtin_eval(a);}
    if (strcmp("join", func) == 0 ) { return builtin_join(a);}
    if (strcmp("init", func) == 0 ) { return builtin_init(a);}
    if (strcmp("len",  func) == 0 ) { return builtin_len(a);}
    if (strcmp("cons",  func) == 0 ) { return builtin_cons(a);}
    if (strstr("+-/*%", func)) { return builtin_op(a, func);}
    lval_del(a);
    return lval_err("Unknow Function!");
}

static lval* builtin_op(lval* a, char* op)
{
    //ensure all arguments are numbers
    for (int i = 0; i < a->count; i++) {
        if(a->cell[i]->type != LVAL_NUM){
            lval_del(a);
            return lval_err("Cannot operator on non number!");
        }
    }

    //pop the first element
    lval* x = lval_pop(a, 0);
    
    if((strcmp(op, "-") == 0) && a->count == 0) { x->num = -x->num;}
    
    while(a->count > 0){
        lval* y = lval_pop(a, 0);
        
        if(strcmp(op, "+") == 0 ) {x->num += y->num;}
        if(strcmp(op, "-") == 0 ) {x->num -= y->num;}
        if(strcmp(op, "*") == 0 ) {x->num *= y->num;}
        if(strcmp(op, "/") == 0 ) {
            if(y->num == 0){
                lval_del(x); lval_del(y);
                //lval_del(a);
                x = lval_err("Division by Zero!"); break;
            }else{
                x->num /= y->num;
            }
        }
        if(strcmp(op, "%") == 0 ) {
            if(y->num == 0){
                lval_del(x); lval_del(y);
                //lval_del(a);
                x = lval_err("Division by Zero!"); break;
            }else{
                x->num %= y->num;
            }
        }

        //delete the element now finished with
        lval_del(y);
    }
    
    //delete input expression and return result
    lval_del(a);
    return x;
}

static lval* builtin_head(lval* a)
{
    //check error conditions
    if(a->count != 1){  
        lval_del(a);
        return lval_err("Function 'head' passed too many arguments!");
    }
    
    if(a->cell[0]->type != LVAL_QEXPR) {
        lval_del(a);
        return lval_err("Function 'head' passed incorrect types!");
    }

    if(a->cell[0]->count == 0) { 
        lval_del(a);
        return lval_err("Function 'head' passed {}");
    }

    lval* v = lval_take(a, 0);
    
    //delete all elements that are not head
    while(v->count > 1) {lval_del(lval_pop(v,1));}
    
    return v;
}

static lval* builtin_tail(lval* a)
{
    LASSERT(a, (a->count == 1), "Function 'tail' passed too many arguments!");
    LASSERT(a, (a->cell[0]->type == LVAL_QEXPR), "Function 'tail' passed incorrect type!");
    LASSERT(a, (a->cell[0]->count != 0), "Function 'tail' passed {}!");
    
    lval* v = lval_take(a, 0);
    lval_del(lval_pop(v, 0));
    return v;
}

static lval* builtin_init(lval* a)
{
    LASSERT(a, (a->count == 1), "Function 'init' passed too many arguments!");
    LASSERT(a, (a->cell[0]->type == LVAL_QEXPR), "Function 'init' passed incorrect type!");
    LASSERT(a, (a->cell[0]->count != 0), "Function 'init' passed {}!");
    
    lval* x = lval_take(a, 0);
    lval_del(lval_pop(x, x->count - 1));
    return x;
}

static lval* builtin_len(lval* a)
{
    LASSERT(a, (a->count == 1), "Function 'len' passed too many arguments!");
    LASSERT(a, (a->cell[0]->type == LVAL_QEXPR), "Function 'len' passed incorrect type!");
    
    lval* x = lval_num(a->cell[0]->count);
    lval_del(a);
    return x;
}


static lval* builtin_cons(lval* a)
{
    LASSERT(a, (a->count == 2), "Function 'cons' should pass two arguments!");
    LASSERT(a, (a->cell[1]->type == LVAL_QEXPR), "Function 'cons' passed incorrect type!");
    
    lval* x = lval_pop(a, 1);
    x = lval_add_front(x, lval_take(a, 0)); 
    return x;
}

static lval* builtin_list(lval* a)
{
    a->type = LVAL_QEXPR;
    return a;
}

static lval* builtin_eval(lval* a)
{
    LASSERT(a, (a->count == 1), "Function 'eval' passed too many arguments!");
    LASSERT(a, (a->cell[0]->type == LVAL_QEXPR), "Function 'eval' passed incorrect type!");
    
    lval* x = lval_take(a,0);
    x->type = LVAL_SEXPR;
    return lval_eval(x);
}

static lval* lval_join(lval* x, lval* y)
{
    // for each cell in 'y' add it to 'x'
    while(y->count){
        x = lval_add(x, lval_pop(y, 0));
    }
    
    // delete the empty 'y' and return 'x'
    lval_del(y);
    return x;
}

static lval* builtin_join(lval* a)
{
    for(int i = 0; i < a->count; i++) {
        LASSERT(a, (a->cell[i]->type == LVAL_QEXPR), "Function 'join' passed incorrect type.");
    }
    
    lval* x = lval_pop(a, 0);
    
    while(a->count) {
        x = lval_join(x, lval_pop(a, 0));
    }
    
    lval_del(a);
    return x;
}

/*
  static lval eval_op(lval x, char * op, lval y)
  {
  if(x.type == LVAL_ERR) return x;
  if(y.type == LVAL_ERR) return y;

  if(strcmp(op, "+") == 0 ) {return lval_num(x.num + y.num);}
  if(strcmp(op, "-") == 0 ) {return lval_num(x.num - y.num);}
  if(strcmp(op, "*") == 0 ) {return lval_num(x.num * y.num);}
  if(strcmp(op, "/") == 0 ) {
  return y.num == 0 ? lval_err(LERR_DIV_ZERO) : lval_num(x.num / y.num);}
  if(strcmp(op, "%") == 0 ) {
  return y.num == 0 ? lval_err(LERR_DIV_ZERO) : lval_num(x.num % y.num);}
  if(strcmp(op, "^") == 0 ) {return lval_num(pow(x.num, y.num));}
  if(strcmp(op, "min") == 0 ){return lval_num(x.num > y.num ? y.num : x.num);} 
  if(strcmp(op, "max") == 0 ){return lval_num(x.num > y.num ? x.num : y.num);}

  return lval_err(LERR_BAD_OP);
  }

  static lval eval(mpc_ast_t * t)
  {
  if(strstr(t->tag, "number")){
  //check if there is some error in conversion
  long x = strtol(t->contents, NULL, 10);
  return errno != ERANGE ? lval_num(x) : lval_err(LERR_BAD_NUM); 
  }
    
  //the operator is always second child.
  char * op = t->children[1]->contents;

  lval x = eval(t->children[2]);

  int i = 3;
  while(strstr(t->children[i]->tag, "expr")){
  x = eval_op(x, op, eval(t->children[i]));
  i ++;
  }
   
  //when operator '-' receives one argument, it negates it.
  if(i == 3 && strcmp(op, "-") == 0 && x.type == LVAL_NUM ){ 
  x.num = -x.num;
  }

  return x;
  }
*/

int main(int argc, char ** argv){
    // create some parsers
    mpc_parser_t* Number = mpc_new("number");
    mpc_parser_t* Symbol = mpc_new("symbol");
    mpc_parser_t* Sexpr = mpc_new("sexpr");
    mpc_parser_t* Qexpr = mpc_new("qexpr");
    mpc_parser_t* Expr = mpc_new("expr");
    mpc_parser_t* Lispy = mpc_new("lispy");

    // define them with following language
    mpca_lang(MPC_LANG_DEFAULT,
              "                                                               \
              number  : /-?[0-9]+/;                                           \
              symbol  : /[a-zA-Z0-9+\\-*\\/\\\\=<>!&]+/;                      \
              sexpr   : '(' <expr>* ')';                                      \
              qexpr   : '{' <expr>* '}';                                      \
              expr    : <number> | <symbol> | <sexpr> | <qexpr>;              \
              lispy   : /^/ <expr>* /$/;                                      \
              ",
              Number, Symbol, Sexpr, Qexpr, Expr, Lispy);

    puts("Lispy Version 0.0.0.0.1");
    puts("Press Ctl+c to Exit\n");

    while(1) {
        char * input =readline("lispy> ");
        add_history(input);

        mpc_result_t r ;
        if(mpc_parse("<stdin>", input, Lispy, &r) == 1){
            mpc_ast_print(r.output);
            lval* result = lval_read(r.output);
            lval_println(result);
            lval* x = lval_eval(result); 
            lval_println(x);
            lval_del(x);

            mpc_ast_delete(r.output);
        }else{
            mpc_err_print(r.error);
            mpc_err_delete(r.error);
        }
        free(input);
    }

    mpc_cleanup(4, Number, Symbol, Sexpr, Qexpr, Expr, Lispy);

    return 0;

}
