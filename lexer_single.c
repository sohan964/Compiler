#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdbool.h>

/* ---- token.h ---- */
typedef enum {
    T_EOF = 0, T_ERROR,
    T_KEYWORD, T_IDENTIFIER, T_NUMBER,
    T_LPAREN, T_RPAREN, T_LBRACE, T_RBRACE, T_SEMI, T_COMMA,
    T_ASSIGN, T_EQ, T_NEQ, T_LT, T_LTE, T_GT, T_GTE,
    T_PLUS, T_MINUS, T_MUL, T_DIV
} TokenType;

typedef struct {
    TokenType type;
    char      lexeme[256];
    int       line;
    int       col;
} Token;

const char* token_type_name(TokenType t) {
    switch (t) {
        case T_EOF: return "EOF";
        case T_ERROR: return "Error";
        case T_KEYWORD: return "keyword";
        case T_IDENTIFIER: return "identifier";
        case T_NUMBER: return "num";
        case T_LPAREN: return "("; case T_RPAREN: return ")";
        case T_LBRACE: return "{"; case T_RBRACE: return "}";
        case T_SEMI: return ";";   case T_COMMA: return ",";
        case T_ASSIGN: return "="; case T_EQ: return "==";
        case T_NEQ: return "!=";   case T_LT: return "<";
        case T_LTE: return "<=";   case T_GT: return ">";
        case T_GTE: return ">=";   case T_PLUS: return "+";
        case T_MINUS: return "-";  case T_MUL: return "*";
        case T_DIV: return "/";
        default: return "??";
    }
}

/* ---- scanner.h/.c (combined) ---- */
static FILE *g_in = NULL;
static int g_line = 1, g_col = 0;
static int g_have_push = 0, g_push_ch = 0;

static const char *KW[] = {
    "void","int","for","while","if","else","return","float","double","char","bool","print",
    NULL
};

static int getch(void){
    int ch;
    if (g_have_push){ g_have_push=0; ch=g_push_ch; }
    else ch = fgetc(g_in);
    if (ch == '\n'){ g_line++; g_col = 0; }
    else if (ch != EOF){ g_col++; }
    return ch;
}
static void ungetch(int ch){
    if (ch==EOF) return;
    g_have_push=1; g_push_ch=ch;
    if (ch=='\n'){ g_line--; } else { g_col--; if (g_col<0) g_col=0; }
}
static void set_token(Token *t, TokenType tp, const char *lex, int line, int col){
    t->type=tp; t->line=line; t->col=col;
    if (lex){ strncpy(t->lexeme, lex, sizeof(t->lexeme)-1); t->lexeme[255]='\0'; }
    else t->lexeme[0]='\0';
}
static void append_char(char *b, size_t cap, size_t *len, int ch){
    if (*len+1 < cap){ b[*len]=(char)ch; (*len)++; b[*len]='\0'; }
}
static bool is_keyword(const char *s){
    for (int i=0; KW[i]; ++i) if (!strcmp(KW[i],s)) return true;
    return false;
}
void scanner_init(FILE *in){ g_in=in; g_line=1; g_col=0; g_have_push=0; g_push_ch=0; }

static int skip_ws_and_comments(void){
    int ch;
    while (1){
        ch=getch(); if (ch==EOF) return EOF;
        if (isspace(ch)) continue;
        if (ch=='/'){
            int ch2=getch();
            if (ch2=='/'){ while (ch!='\n' && ch!=EOF) ch=getch(); continue; }
            else if (ch2=='*'){
                int prev=0, cur; bool closed=false;
                while ((cur=getch())!=EOF){ if (prev=='*' && cur=='/'){ closed=true; break; } prev=cur; }
                if (!closed) return EOF;
                continue;
            } else { ungetch(ch2); return '/'; }
        }
        return ch;
    }
}
static TokenType scan_identifier_or_keyword(int first, Token *out, int L, int C){
    char buf[256]={0}; size_t len=0; append_char(buf,256,&len,first);
    int ch; while ((ch=getch())!=EOF && (isalnum(ch)||ch=='_')) append_char(buf,256,&len,ch);
    if (ch!=EOF) ungetch(ch);
    set_token(out, is_keyword(buf)?T_KEYWORD:T_IDENTIFIER, buf, L, C);
    return out->type;
}
static TokenType scan_number(int first, Token *out, int L, int C){
    char buf[256]={0}; size_t len=0; append_char(buf,256,&len,first);
    int ch;
    while ((ch=getch())!=EOF && isdigit(ch)) append_char(buf,256,&len,ch);
    if (ch=='.'){
        append_char(buf,256,&len,ch);
        int ch2=getch();
        if (isdigit(ch2)){ append_char(buf,256,&len,ch2);
            while ((ch=getch())!=EOF && isdigit(ch)) append_char(buf,256,&len,ch);
        } else { ch=ch2; }
    }
    if (ch=='e'||ch=='E'){
        int save_ch=ch; append_char(buf,256,&len,ch);
        int ch2=getch(); if (ch2=='+'||ch2=='-'){ append_char(buf,256,&len,ch2); ch2=getch(); }
        if (!isdigit(ch2)){ /* malformed exponent: drop E back */
            if (len) { len--; buf[len]='\0'; }  /* remove E */
            ungetch(save_ch);
            if (ch2!=EOF) ungetch(ch2);
            set_token(out, T_NUMBER, buf, L, C); return T_NUMBER;
        }
        append_char(buf,256,&len,ch2);
        while ((ch=getch())!=EOF && isdigit(ch)) append_char(buf,256,&len,ch);
    }
    if (ch!=EOF) ungetch(ch);
    set_token(out, T_NUMBER, buf, L, C); return T_NUMBER;
}
TokenType next_token(Token *out){
    int ch = skip_ws_and_comments();
    if (ch==EOF){ set_token(out,T_EOF,"EOF",g_line,g_col); return T_EOF; }
    int L=g_line, C=(g_col>0?g_col:1);
    if (isalpha(ch)||ch=='_') return scan_identifier_or_keyword(ch,out,L,C);
    if (isdigit(ch)) return scan_number(ch,out,L,C);
    if (ch=='.'){ set_token(out,T_ERROR,".",L,C); return T_ERROR; }
    switch (ch){
        case '(': set_token(out,T_LPAREN,"(",L,C); return T_LPAREN;
        case ')': set_token(out,T_RPAREN,")",L,C); return T_RPAREN;
        case '{': set_token(out,T_LBRACE,"{",L,C); return T_LBRACE;
        case '}': set_token(out,T_RBRACE,"}",L,C); return T_RBRACE;
        case ';': set_token(out,T_SEMI,";",L,C);   return T_SEMI;
        case ',': set_token(out,T_COMMA,",",L,C);  return T_COMMA;
        case '+': set_token(out,T_PLUS,"+",L,C);   return T_PLUS;
        case '-': set_token(out,T_MINUS,"-",L,C);  return T_MINUS;
        case '*': set_token(out,T_MUL,"*",L,C);    return T_MUL;
        case '/': set_token(out,T_DIV,"/",L,C);    return T_DIV;
        case '=':{ int c2=getch(); if (c2=='='){ set_token(out,T_EQ,"==",L,C); return T_EQ; }
                   ungetch(c2); set_token(out,T_ASSIGN,"=",L,C); return T_ASSIGN; }
        case '!':{ int c2=getch(); if (c2=='='){ set_token(out,T_NEQ,"!=",L,C); return T_NEQ; }
                   ungetch(c2); set_token(out,T_ERROR,"!",L,C);  return T_ERROR; }
        case '<':{ int c2=getch(); if (c2=='='){ set_token(out,T_LTE,"<=",L,C); return T_LTE; }
                   ungetch(c2); set_token(out,T_LT,"<",L,C);     return T_LT; }
        case '>':{ int c2=getch(); if (c2=='='){ set_token(out,T_GTE,">=",L,C); return T_GTE; }
                   ungetch(c2); set_token(out,T_GT,">",L,C);     return T_GT; }
        default: { char tmp[2]={(char)ch,'\0'}; set_token(out,T_ERROR,tmp,L,C); return T_ERROR; }
    }
}

/* ---- main.c ---- */
static void print_token(const Token *t){
    const char *tn = token_type_name(t->type);
    if (t->type==T_KEYWORD || t->type==T_IDENTIFIER || t->type==T_NUMBER || t->type==T_ERROR)
        printf("%s : %s\n", tn, t->lexeme);
    else
        printf("%s : %s\n", tn, tn);
}
int main(int argc, char **argv){
    if (argc<2){ fprintf(stderr,"Usage: %s <input-file>\n", argv[0]); return 1; }
    FILE *f=fopen(argv[1],"r"); if (!f){ perror("fopen"); return 1; }
    scanner_init(f);
    Token tk;
    while (1){
        TokenType tp = next_token(&tk);
        if (tp==T_EOF) break;
        print_token(&tk);
    }
    fclose(f);
    return 0;
}
