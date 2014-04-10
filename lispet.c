#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "mpc.h"
#include <math.h>

#define LASSERT(args, cond, fmt, ...) \
    if (!(cond)) {lval* err = lval_err(fmt, ##__VA_ARGS__); lval_del(args); return err;}

#define LASSERT_TYPE(func, args, index, expect) \
    LASSERT(args, args->cell[index]->type == expect,    \
        "Function '%s' passed incorrect type for argument %i. Got %s, Expected %s.", \
            func, index, ltype_name(args->cell[index]->type), ltype_name(expect)) 
        
#define LASSERT_NUM(func, args, expect)                \
    LASSERT(args, args->count == expect, \
        "Function '%s' passed incorrect number of arguments. Got %i, Expected %i.", \
            func, args->count, expect)

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

//Forward Declarations
struct lval;
struct lenv;
typedef struct lval lval;
typedef struct lenv lenv;

//Lval Types
enum {LVAL_NUM, LVAL_ERR, LVAL_SYM, LVAL_FUN, LVAL_SEXPR, LVAL_QEXPR};

typedef lval* (*lbuiltin)(lenv*, lval*);

struct lval{
    int type;

    // Basic
    long num;
    char * err;
    char * sym;

    // Function
    lbuiltin builtin;
    lenv* env;
    lval* formals;
    lval* body;

    // Expression
    int count;
    lval ** cell;
};

struct lenv{
    lenv* par;
    int count;
    char ** syms;
    lval** vals;
};

static void lval_del(lval* v);
static lval* lval_err(char * fmt, ...);
static lval* lval_copy(lval* v);

static lval* lval_pop(lval* v, int i);
static lval* lval_take(lval* v, int i);

static char * ltype_name(int t)
{
    switch(t) {
    case LVAL_FUN: return "Function";
    case LVAL_NUM: return "Number";
    case LVAL_ERR: return "Error";
    case LVAL_SYM: return "Symbol";
    case LVAL_SEXPR: return "S-Expression";
    case LVAL_QEXPR: return "Q-Expression";
    default: return "Unknown";
    }
}

// construction functions
static lenv* lenv_new(void)
{
    lenv* e = malloc(sizeof(lenv));
    e->par = NULL;
    e->count = 0;
    e->syms = NULL;
    e->vals = NULL;
    return e;
}

static void lenv_del(lenv* e)
{
    for (int i = 0; i < e->count; i++) {
        free(e->syms[i]);
        lval_del(e->vals[i]);
    }

    free(e->syms);
    free(e->vals);
    free(e);
}

static lval* lenv_get(lenv* e, lval* k)
{
    for (int i = 0; i < e->count; i++) {
        if(strcmp(e->syms[i], k->sym) == 0) { return lval_copy(e->vals[i]);} 
    }

    //if no symbol found, return error
    if(e->par){
        return lenv_get(e->par, k);
    }else{
        return lval_err("unbound symbol '%s'!", k->sym);
    }
} 

static void lenv_put(lenv* e, lval* k, lval* v)
{
    //Check if variable already exists
    for (int i = 0; i < e->count; i++) {
        if(strcmp(e->syms[i], k->sym) == 0) {
            lval_del(e->vals[i]);
            e->vals[i] = lval_copy(v);
            // Why we need next two line? They are necessary.
            e->syms[i] = realloc(e->syms[i], strlen(k->sym) + 1);
            strcpy(e->syms[i], k->sym);
            return;
        }
    }

    //if no existing entry found then allocate space for new entry
    e->count++;
    e->vals = realloc(e->vals, sizeof(lval*) * e->count);
    e->syms = realloc(e->syms, sizeof(char*) * e->count);
    e->vals[e->count-1] = lval_copy(v);
    // Why we need next two line? They are necessary.
    e->syms[e->count-1] = malloc(strlen(k->sym) + 1);
    strcpy(e->syms[e->count-1], k->sym);
}

static lval* lval_lambda(lval* formals, lval* body)
{
    lval* v = malloc(sizeof(lval));
    v->type = LVAL_FUN;

    v->builtin = NULL;

    //build new enviroment
    v->env = lenv_new();
    
    //set formals and body
    v->formals = formals;
    v->body = body;
    return v;
}

static lval* lval_num(long x)
{
    lval* v = malloc(sizeof(lval));
    v->type = LVAL_NUM;
    v->num = x;
    return v;
}

static lval* lval_err(char * fmt, ...)
{
    lval* v = malloc(sizeof(lval));
    v->type = LVAL_ERR;

    //create a va list and initialize it
    va_list va;
    va_start(va, fmt);

    v->err = malloc(512);

    vsnprintf(v->err, 511, fmt, va);

    v->err = realloc(v->err, strlen(v->err) + 1);
	
    va_end(va);	

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

static lval* lval_fun(lbuiltin func)
{
    lval* v = malloc(sizeof(lval));
    v->type = LVAL_FUN;
    v->builtin = func;
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
    case LVAL_FUN: 
        if(!v->builtin){
            lenv_del(v->env);
            lval_del(v->formals);
            lval_del(v->body);
        }
        break;
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

static lenv* lenv_copy(lenv* e)
{
    lenv* n = malloc(sizeof(lenv));
    n->par = e->par;
    n->count = e->count;
    n->syms = malloc(sizeof(char *) * n->count);
    n->vals = malloc(sizeof(lval*) * n->count);
    for (int i = 0; i < e->count; i++) {
        n->syms[i] = malloc(strlen(e->syms[i]) + 1);
        strcpy(n->syms[i], e->syms[i]);
        n->vals[i] = lval_copy(e->vals[i]);
    }
    return n;
}

static lval* lval_copy(lval* v)
{
    lval* x = malloc(sizeof(lval));
    x->type = v->type;
    
    switch (v->type) {
    case LVAL_FUN:
        if(v->builtin){
            x->builtin = v->builtin; 
        }else {
            x->env = lenv_copy(v->env);
            x->formals = lval_copy(v->formals);
            x->body = lval_copy(v->body);
        }
        break;
    case LVAL_NUM: x->num = v->num; break;

    case LVAL_ERR: x->err = malloc(strlen(v->err) + 1); strcpy(x->err, v->err);break;
    case LVAL_SYM: x->sym = malloc(strlen(v->sym) + 1); strcpy(x->sym, v->sym);break;
        
    case LVAL_SEXPR:
    case LVAL_QEXPR:
        x->count = v->count;
        x->cell = malloc(sizeof(lval*) * x->count);
        for(int i = 0; i < x->count; i++){
            x->cell[i] = lval_copy(v->cell[i]);
        }
        break;
    }
    return x;
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
    return errno != ERANGE ? lval_num(x) : lval_err("invalid number '%s'", t->contents); 
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
    case LVAL_FUN: 
        if(v->builtin){
        printf("<function>");
        }else{
            printf("(\\");lval_print(v->formals); putchar(' '); lval_print(v->body);putchar(')');
        } break; 
    case LVAL_SEXPR: lval_expr_print(v, '(', ')'); break; 
    case LVAL_QEXPR: lval_expr_print(v, '{', '}'); break; 
    }
}

static void lval_println(lval* v) { lval_print(v); putchar('\n');}

static lval* lval_eval(lenv* e, lval* v);

static lval* lval_eval_sexpr(lenv* e, lval* v) 
{
    //evaluation children
    for (int i = 0; i < v->count; i++) {
        v->cell[i] = lval_eval(e, v->cell[i]); 
    }

    //error checking
    for (int i = 0; i < v->count; i++) {
        if(v->cell[i]->type == LVAL_ERR) { return lval_take(v,i);} 
    }

    //empty expression
    if(v->count == 0) { return v;}

    //single expression, here we assume that functions at least have
    //an argument.
    if(v->count == 1){ return lval_take(v, 0);}

    //ensure first element is symbol
    lval* f = lval_pop(v, 0);
    if(f->type != LVAL_FUN){
        lval* err = lval_err("first element '%s' is not a function!", 
                             ltype_name(f->type));
        lval_del(f); lval_del(v);
        return err;
    }
    
    //call builtin with operator
    lval* result = f->builtin(e, v);
    lval_del(f);
    return result;
}

static lval* lval_eval(lenv* e, lval* v)
{
    if(v->type == LVAL_SYM){
        lval* x = lenv_get(e,v);
        lval_del(v);
        return x;
    }

    if(v->type == LVAL_SEXPR) { return lval_eval_sexpr(e, v);}
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

   

static lval* builtin_op(lenv* e, lval* a, char* op)
{
    //ensure all arguments are numbers
    for (int i = 0; i < a->count; i++) {LASSERT_TYPE(op, a, i, LVAL_NUM);}

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

static lval* builtin_add(lenv* e, lval* a) { return builtin_op(e, a, "+");}
static lval* builtin_sub(lenv* e, lval* a) { return builtin_op(e, a, "-");}
static lval* builtin_mul(lenv* e, lval* a) { return builtin_op(e, a, "*");}
static lval* builtin_div(lenv* e, lval* a) { return builtin_op(e, a, "/");}
 
static lval* builtin_head(lenv* e, lval* a)
{
    LASSERT_NUM("head", a, 1);
    LASSERT_TYPE("head", a, 0, LVAL_QEXPR);
    LASSERT(a, (a->cell[0]->count != 0), "Function 'head' passed {}!");
 
    lval* v = lval_take(a, 0);
    
    //delete all elements that are not head
    while(v->count > 1) {lval_del(lval_pop(v,1));}
    
    return v;
}

static lval* builtin_tail(lenv* e, lval* a)
{
    LASSERT_NUM("tail", a, 1);
    LASSERT_TYPE("tail", a, 0, LVAL_QEXPR);
    LASSERT(a, (a->cell[0]->count != 0), "Function 'tail' passed {}!");
    
    lval* v = lval_take(a, 0);
    lval_del(lval_pop(v, 0));
    return v;
}

static lval* builtin_init(lenv* e, lval* a)
{
    LASSERT_NUM("init", a, 1);
    LASSERT_TYPE("init", a, 0, LVAL_QEXPR);
    LASSERT(a, (a->cell[0]->count != 0), "Function 'init' passed {}!");
    
    lval* x = lval_take(a, 0);
    lval_del(lval_pop(x, x->count - 1));
    return x;
}

static lval* builtin_len(lenv* e, lval* a)
{
    LASSERT_NUM("len", a, 1);
    LASSERT_TYPE("len", a, 0, LVAL_QEXPR);
    
    lval* x = lval_num(a->cell[0]->count);
    lval_del(a);
    return x;
}

static lval* builtin_cons(lenv* e, lval* a)
{
    LASSERT_NUM("cons", a, 2);
    LASSERT_TYPE("cons", a, 1, LVAL_QEXPR);
    
    lval* x = lval_pop(a, 1);
    x = lval_add_front(x, lval_take(a, 0)); 
    return x;
}

static lval* builtin_list(lenv* e, lval* a)
{
    a->type = LVAL_QEXPR;
    return a;
}

static lval* builtin_eval(lenv* e, lval* a)
{
    LASSERT_NUM("eval", a, 1);
    LASSERT_TYPE("eval", a, 0, LVAL_QEXPR);
    
    lval* x = lval_take(a,0);
    x->type = LVAL_SEXPR;
    return lval_eval(e, x);
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

static lval* builtin_join(lenv* e, lval* a)
{
    for(int i = 0; i < a->count; i++) {LASSERT_TYPE("join", a, i, LVAL_QEXPR);}
    
    lval* x = lval_pop(a, 0);
    
    while(a->count) {
        x = lval_join(x, lval_pop(a, 0));
    }
    
    lval_del(a);
    return x;
}

static lval* builtin_def(lenv* e, lval* a)
{
    LASSERT_TYPE("def", a, 0, LVAL_QEXPR);
    
    //first element is symbol list
    lval* syms = a->cell[0];

    for (int i = 0; i < syms->count; i++) {
        LASSERT(a, (syms->cell[i]->type == LVAL_SYM), "Function 'def' cannot define non-symbol!");
    }

    //check correct number of symbols and values
    LASSERT(a, (syms->count == a->count-1), "Function 'def' cannot define incorrect number of values to symbols!");

    for (int i = 0; i < syms->count; i++) {
        lenv_put(e, syms->cell[i], a->cell[i+1]);
    }

    lval_del(a);
    return lval_sexpr();
}

static lval* builtin_exit(lenv* e, lval* a)
{
    exit(EXIT_SUCCESS); 
}

static lval* builtin_lambda(lenv* e, lval* a)
{
    //check two arguments, each of which are Q-expression
    LASSERT_NUM("\\", a, 2);
    LASSERT_TYPE("\\", a, 0, LVAL_QEXPR);
    LASSERT_TYPE("\\", a, 1, LVAL_QEXPR);
    
    //check first Q-expression contains only Symbols
    for (int i = 0; i <  a->cell[0]->count; i++) {
        LASSERT(a, (a->cell[0]->cell[i]->type == LVAL_SYM),
                "Cannot define non-symbol. Got %s, Expected %s.",
                ltype_name(a->cell[0]->cell[i]->type), ltype_name(LVAL_QEXPR));
    }
    lval* formals = lval_pop(a, 0);
    lval* body = lval_pop(a, 0);
    lval_del(a);

    return lval_lambda(formals, body);
}

static void lenv_add_builtin(lenv* e, char* name, lbuiltin func)
{
    lval* k = lval_sym(name);
    lval* v = lval_fun(func);
    lenv_put(e, k, v);
    lval_del(k); lval_del(v);
}

static void lenv_add_builtins(lenv *e)
{
    //list functions
    lenv_add_builtin(e, "list", builtin_list);
    lenv_add_builtin(e, "head", builtin_head);
    lenv_add_builtin(e, "tail", builtin_tail);
    lenv_add_builtin(e, "eval", builtin_eval);
    lenv_add_builtin(e, "join", builtin_join);
    lenv_add_builtin(e, "cons", builtin_cons);
    lenv_add_builtin(e, "init", builtin_init);
    lenv_add_builtin(e, "len", builtin_len);
    lenv_add_builtin(e, "def", builtin_def);
    lenv_add_builtin(e, "exit", builtin_exit);
    
    //mathematical functions
    lenv_add_builtin(e, "+", builtin_add);
    lenv_add_builtin(e, "-", builtin_sub);
    lenv_add_builtin(e, "*", builtin_mul);
    lenv_add_builtin(e, "/", builtin_div);
} 

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
    
    lenv* e =lenv_new();
    lenv_add_builtins(e);

    while(1) {
        char * input =readline("lispy> ");
        add_history(input);

        mpc_result_t r ;
        if(mpc_parse("<stdin>", input, Lispy, &r) == 1){
            //mpc_ast_print(r.output);
            lval* result = lval_read(r.output);
            //lval_println(result);
            lval* x = lval_eval(e, result); 
            lval_println(x);
            lval_del(x);

            mpc_ast_delete(r.output);
        }else{
            mpc_err_print(r.error);
            mpc_err_delete(r.error);
        }
        free(input);
    }
    
    lenv_del(e);

    mpc_cleanup(4, Number, Symbol, Sexpr, Qexpr, Expr, Lispy);

    return 0;

}
