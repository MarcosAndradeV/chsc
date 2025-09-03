/// INCLUDES
#include <assert.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#define NOB_IMPLEMENTATION
#define NOB_STRIP_PREFIX
#include "nob.h"
#define FLAG_IMPLEMENTATION
#include "flag.h"
// #define STB_DS_IMPLEMENTATION
// #include "stb_ds.h"

/// MACROS
#define try(expr) if(!expr) return false
#define defer_try(expr) if(!expr) return_defer(false)

/// CONSTANTS
#define VERSION "0.1.0"

/// LEXER_TYPES
typedef enum {
    TOKEN_EOF = 0,
    TOKEN_INVALID,
    TOKEN_UNEXPECTED_CHARACTER,
    TOKEN_INVALID_ESCAPE_SEQUENCE,
    TOKEN_UNTERMINATED_STRING_LITERAL,
    TOKEN_UNKNOW_INTERGER_LITERAL,
    TOKEN_OPEN_PAREN,
    TOKEN_CLOSE_PAREN,
    TOKEN_OPEN_BRACKET,
    TOKEN_CLOSE_BRACKET,
    TOKEN_OPEN_BRACE,
    TOKEN_CLOSE_BRACE,
    TOKEN_MACRO_CALL,
    TOKEN_IDENTIFIER,
    TOKEN_IMPORT,
    TOKEN_FN,
    TOKEN_KEYWORD,
    TOKEN_REAL_NUMBER,
    TOKEN_INTEGER_NUMBER,
    TOKEN_U8_NUMBER,
    TOKEN_I8_NUMBER,
    TOKEN_U16_NUMBER,
    TOKEN_I16_NUMBER,
    TOKEN_U32_NUMBER,
    TOKEN_I32_NUMBER,
    TOKEN_U64_NUMBER,
    TOKEN_I64_NUMBER,
    TOKEN_STRING_LITERAL,
    TOKEN_CHARACTER_LITERAL,
    TOKEN_DOT,
    TOKEN_TILDE,
    TOKEN_SPLAT,
    TOKEN_COMMA,
    TOKEN_COLON,
    TOKEN_DOUBLE_COLON,
    TOKEN_SEMICOLON,
    TOKEN_ARROW,
    TOKEN_BACK_SLASH,
    TOKEN_CARET,
    TOKEN_ASSIGN,
    TOKEN_BANG,
    TOKEN_PLUS,
    TOKEN_MINUS,
    TOKEN_ASTERISK,
    TOKEN_SLASH,
    TOKEN_PERCENT,
    TOKEN_EQ,
    TOKEN_NOT_EQ,
    TOKEN_GT,
    TOKEN_GT_EQ,
    TOKEN_LT,
    TOKEN_LT_EQ,
    TOKEN_AMPERSAND,
    TOKEN_PIPE,
    TOKEN_DOUBLE_AMPERSAND,
    TOKEN_DOUBLE_PIPE,
    TOKEN_DOLLAR,
    TOKEN_SHIFT_LEFT,
    TOKEN_SHIFT_RIGHT,
    TOKEN_PUNCT,
} TokenKind;

typedef struct {
    const char* file_path;
    size_t line;
    size_t col;
} Loc;

#define LOCFmt "%s:%ld:%ld"
#define LOCArgs(loc) (loc).file_path, (loc).line, (loc).col

typedef struct {
    TokenKind kind;
    Loc loc;
    String_View source;
} Token;

static String_Builder sources;

typedef struct {
    String_View source;
    size_t pos;
    Loc loc;
    String_View token;
} LexerT;

static LexerT Lexer;

/// Ops

typedef enum {
    OP_BEGIN_FN,

    OP_BEGIN_FN_ARGS,
    OP_END_FN_ARGS,

    OP_BEGIN_BLOCK,
    OP_END_BLOCK,

    OP_BEGIN_ARGS,
    OP_END_ARGS,

    OP_PUSH_CALL,
    OP_PUSH_IDENT,
    OP_PUSH_STRLIT,
    OP_PUSH_INTLIT,

    OP_PLUS,
} OpKind;

typedef struct {
    OpKind kind;
    Loc loc;
    union {
        size_t operand;
        String_View literal;
    };
} Op;

static struct {
    Op *items;
    size_t capacity, count;
} Ops;

#define Ops_append(...) da_append(&Ops, ((Op){__VA_ARGS__}))

/// GENERAL_FUNCTIONS
void usage(FILE *stream);
bool parse_program(const char* input_path);
bool parse_args();
bool parse_body();
bool parse_call_args();
bool parse_expression(TokenKind kind);
void sources_free();

/// LEXER_IMPLEMENTATION
bool Lexer_load_file(const char* input_path);
TokenKind Lexer_next();
LexerT Lexer_save();
void Lexer_restore(LexerT pos);

/// MAIN
int main(int argc, char **argv) {
    bool *compile = flag_bool("compile", false, "Compile program: -compile <file.chs>");
    bool *run = flag_bool("run", false, "Run program: -run <file.chs>");
    bool *help = flag_bool("help", true, "Print this help to stdout and exit with 0");
    bool *version = flag_bool("version", false, "Print the version of the chsc and exit with 0");

    if (!flag_parse(argc, argv)) {
        usage(stderr);
        flag_print_error(stderr);
        exit(1);
    }

    int rest_argc = flag_rest_argc();
    char **rest_argv = flag_rest_argv();

    if(*compile) {
        if (rest_argc <= 0) {
            fprintf(stderr, "ERROR: no input file\n");
            exit(1);
        }
        const char* input_path = shift(rest_argv, rest_argc);

        try(parse_program(input_path));

        // da_free(sources);
        TODO("-compile");
    }

    if(*run) {
        TODO("-run");
    }

    if(*version) {
        printf("chsc: %s\n", VERSION);
        exit(0);
    }

    if (*help) {
        usage(stdout);
        exit(0);
    }
}


/// IMPLEMENTATIONS

void usage(FILE *stream){
    fprintf(stream, "Usage: ./chsc <command> <arguments>\n");
    fprintf(stream, "Commands:\n");
    flag_print_options(stream);
}

void sources_free() {
    da_free(sources);
}

bool Lexer_load_file(const char* input_path) {
    size_t count = sources.count;
    char* items = sources.items;
    try(read_entire_file(input_path, &sources));
    Lexer.source = sv_from_parts((char*)((size_t)items + sources.items), (count - sources.count));
    Lexer.pos  = 0;
    Lexer.loc = (Loc){
        .file_path = input_path,
        .col = 1,
        .line = 1,
    };
    return true;
}

void Loc_next(Loc *loc, char ch) {
    switch(ch){
        case '\n': {
            loc->line++;
            loc->col = 1;
        } break;
        case '\t':{
            const int ts = 8;
            loc->col = (loc->col / ts) * ts + ts;
        } break;
        default: {
            if(!iscntrl(ch)) loc->col++;
        } break;
    }
}

static inline char Lexer_read_char() {
    size_t pos = Lexer.pos;
    return pos < Lexer.source.count ? Lexer.source.data[pos] : 0;
}

static inline char Lexer_advance() {
    char ch = Lexer_read_char();
    Lexer.pos += 1;
    Loc_next(&Lexer.loc, ch);
    return ch;
}

LexerT Lexer_save() {
    return Lexer;
}

void Lexer_restore(LexerT savepoint) {
    Lexer = savepoint;
}

TokenKind Lexer_next() {
    while(Lexer.pos <= Lexer.source.count) {
        size_t begin = Lexer.pos;
        char ch = Lexer_advance();
        Lexer.token = nob_sv_from_cstr("");

        switch (ch) {
            case 0: return TOKEN_EOF;
            case '#': {
                for(; ch != '\n'; ch = Lexer_advance()) {}
            } break;
            case ',': return TOKEN_COMMA;
            case '+': return TOKEN_PLUS;
            case ';': return TOKEN_SEMICOLON;
            case '(': return TOKEN_OPEN_PAREN;
            case ')': return TOKEN_CLOSE_PAREN;
            case '{': return TOKEN_OPEN_BRACE;
            case '}': return TOKEN_CLOSE_BRACE;
            case '"': {
                for(Lexer_advance(); ch != '"'; ch = Lexer_advance()) {
                    if(ch == 0) {
                        Lexer.token = sv_from_parts(Lexer.source.data + begin, Lexer.pos - begin);
                        return TOKEN_UNTERMINATED_STRING_LITERAL;
                    }
                }
                Lexer.token = sv_from_parts(Lexer.source.data + begin, Lexer.pos - begin);
                return TOKEN_STRING_LITERAL;
            } break;
            default: {
                if(isalpha(ch)) {
                    for(;;) {
                        char ch = Lexer_read_char();
                        if(isalpha(ch) || ch == '_' || isalnum(ch)) Lexer_advance();
                        else break;
                    }
                    String_View token = sv_from_parts(Lexer.source.data + begin, Lexer.pos - begin);
                    TokenKind kind = TOKEN_IDENTIFIER;
                    if(sv_eq(token, sv_from_cstr("import"))) {
                        kind = TOKEN_IMPORT;
                    }
                    else if(sv_eq(token, sv_from_cstr("fn"))) {
                        kind = TOKEN_FN;
                    }
                    Lexer.token = token;
                    return kind;
                } else if(isspace(ch)) {
                    continue;
                } else if(isalnum(ch)) {
                    for(;;) {
                        char ch = Lexer_read_char();
                        if(isalnum(ch)) Lexer_advance();
                        else break;
                    }
                    Lexer.token = sv_from_parts(Lexer.source.data + begin, Lexer.pos - begin);
                    return TOKEN_INTEGER_NUMBER;
                } else {
                    Lexer.token = sv_from_parts(Lexer.source.data + begin, Lexer.pos - begin);
                    return TOKEN_UNEXPECTED_CHARACTER;
                }
            } break;
        }
    }

    return TOKEN_EOF;
}

static struct {
    Op* items;
    size_t capacity, count;
} operator_stack;
static char intlit_buf[32];

bool parse_program(const char* input_path) {
    bool result = true;

    defer_try(Lexer_load_file(input_path));
    for(TokenKind kind = Lexer_next(); kind != TOKEN_EOF; kind = Lexer_next()) {
        switch(kind) {
            case TOKEN_IMPORT: {
                TODO("TOKEN_IMPORT");
            } break;
            case TOKEN_FN: {
                TokenKind kind = Lexer_next(); if(kind != TOKEN_IDENTIFIER) TODO("");
                // TODO: Replace with Arena
                // const char* name = temp_sv_to_cstr(Lexer.token);
                Ops_append(.kind = OP_BEGIN_FN, .loc = Lexer.loc, .literal = Lexer.token);
                defer_try(parse_args());
                defer_try(parse_body());
            } break;
            case TOKEN_UNEXPECTED_CHARACTER:
            default: {
                printf("Token(%d): "SV_Fmt"\n", kind, SV_Arg(Lexer.token));
                TODO("TOKEN_UNEXPECTED_CHARACTER");
            } break;
        }
    }

defer:
    if(operator_stack.items) da_free(operator_stack);
    return result;
}

bool parse_args() {
    TokenKind kind;
    kind = Lexer_next(); if(kind != TOKEN_OPEN_PAREN) {
        printf(LOCFmt": Expect `(`\n", LOCArgs(Lexer.loc));
        return false;
    }
    kind = Lexer_next(); if(kind != TOKEN_CLOSE_PAREN) {
        printf(LOCFmt": Expect `)`\n", LOCArgs(Lexer.loc));
        return false;
    }
    return true;
}

bool parse_body() {
    TokenKind kind;
    kind = Lexer_next(); if(kind != TOKEN_OPEN_BRACE) {
        printf(LOCFmt": Expect `{`\n", LOCArgs(Lexer.loc));
        return false;
    }

    for(TokenKind kind = Lexer_next(); kind != TOKEN_CLOSE_BRACE; kind = Lexer_next()) {
        switch(kind) {
            case TOKEN_IDENTIFIER: {
                Op op = {.kind = OP_PUSH_IDENT, .loc = Lexer.loc, .literal = Lexer.token};
                kind = Lexer_next();
                if(kind == TOKEN_OPEN_PAREN) {
                    op.kind = OP_PUSH_CALL;
                    da_append(&operator_stack, op);
                    Ops_append(.kind = OP_BEGIN_ARGS, .loc = Lexer.loc, .literal = Lexer.token);
                    try(parse_call_args());
                } else {
                    da_append(&Ops, op);
                }
            } break;
            case TOKEN_SEMICOLON: {
                while(operator_stack.count > 0) {
                    da_append(&Ops, da_last(&operator_stack));
                    operator_stack.count--;
                }
            } break;
            case TOKEN_UNEXPECTED_CHARACTER:
            default: {
                printf(LOCFmt": Token(%d): "SV_Fmt"\n", LOCArgs(Lexer.loc), kind, SV_Arg(Lexer.token));
                TODO("TOKEN_UNEXPECTED");
            } break;
        }
    }

    return true;
}

bool parse_call_args() {
    for(TokenKind kind = Lexer_next(); kind != TOKEN_CLOSE_PAREN; kind = Lexer_next()) {
        try(parse_expression(kind));
    }
    if(operator_stack.count > 0 && da_last(&operator_stack).kind == OP_PUSH_CALL) {
        Ops_append(.kind = OP_END_ARGS, .loc = Lexer.loc, .literal = Lexer.token);
        da_append(&Ops, da_last(&operator_stack));
        operator_stack.count--;
    } else {
        printf(LOCFmt": Missmacth `)`: "SV_Fmt"\n", LOCArgs(Lexer.loc), SV_Arg(Lexer.token));
    }
    return true;
}

bool parse_expression(TokenKind kind) {
    switch(kind) {
        case TOKEN_IDENTIFIER: {
            Op op = {.kind = OP_PUSH_IDENT, .loc = Lexer.loc, .literal = Lexer.token};
            kind = Lexer_next();
            if(kind == TOKEN_OPEN_PAREN) {
                op.kind = OP_PUSH_CALL;
                da_append(&operator_stack, op);
                Ops_append(.kind = OP_BEGIN_ARGS, .loc = Lexer.loc, .literal = Lexer.token);
                try(parse_call_args());
            } else {
                da_append(&Ops, op);
            }
        } break;
        case TOKEN_STRING_LITERAL: {
            Ops_append(.kind = OP_PUSH_STRLIT, .loc = Lexer.loc, .literal = Lexer.token);
        } break;
        case TOKEN_UNEXPECTED_CHARACTER:
        default: {
            printf(LOCFmt": Token(%d): "SV_Fmt"\n", LOCArgs(Lexer.loc), kind, SV_Arg(Lexer.token));
            TODO("TOKEN_UNEXPECTED");
        } break;
    }

    return true;
}

/*
case TOKEN_INTEGER_NUMBER: {
    assert(Lexer.token.count < 19 && "TOKEN_INTEGER_NUMBER is too big");
    snprintf(intlit_buf, ARRAY_LEN(intlit_buf), SV_Fmt, SV_Arg(Lexer.token));
    size_t operand = strtoul(intlit_buf, NULL, 10);
    Ops_append(
        .kind = OP_PUSH_INTLIT,
        .loc = Lexer.loc,
        .operand = operand
    );
} break;
case TOKEN_IDENTIFIER: {
    TODO("TOKEN_IDENTIFIER");
} break;

 */
