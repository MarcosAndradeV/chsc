
#include <ctype.h>
#include <stdio.h>
#define NOB_IMPLEMENTATION
#define NOB_STRIP_PREFIX
#include "nob.h"
#define FLAG_IMPLEMENTATION
#include "flag.h"
// #define STB_DS_IMPLEMENTATION
// #include "stb_ds.h"


#define try(expr) if(!expr) return false


#define VERSION "0.1.0"


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
} TokenKind;

typedef struct {
    const char* file_path;
    size_t line;
    size_t col;
} Loc;

typedef struct {
    TokenKind kind;
    Loc loc;
    String_View source;
} Token;

static String_Builder sources;

typedef struct {
    String_Builder source;
    size_t pos;
    Loc loc;
} Lexer;


void usage(FILE *stream);

bool Lexer_init(Lexer *lex, const char* input_path);
void Lexer_deinit(Lexer *lex);
Token Lexer_next(Lexer *lex);
size_t Lexer_save(Lexer *lex);
void Lexer_restore(Lexer *lex, size_t pos);

bool compile_program(const char* input_path);

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

        try(compile_program(input_path));

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

void usage(FILE *stream){
    fprintf(stream, "Usage: ./chsc <command> <arguments>\n");
    fprintf(stream, "Commands:\n");
    flag_print_options(stream);
}

bool Lexer_init(Lexer *lex, const char* input_path) {
    size_t capacity = sources.capacity;
    size_t count = sources.count;
    char* items = sources.items;
    try(read_entire_file(input_path, &sources));
    lex->source.items = (char*)((size_t)items + sources.items);
    lex->source.count = (count - sources.count);
    lex->source.capacity = (capacity - sources.capacity);
    lex->pos  = 0;
    lex->loc = (Loc){
        .file_path = input_path,
        .col = 1,
        .line = 1,
    };
    return true;
}

void Lexer_deinit(Lexer *lex) {
    free(lex->source.items);
    lex->source = (String_Builder){0};
    lex->pos  = 0;
    lex->loc = (Loc){0};
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

static inline char Lexer_read_char(Lexer *lex) {
    size_t pos = lex->pos;
    return pos <= lex->source.count ? lex->source.items[pos] : 0;
}

static inline char Lexer_advance(Lexer *lex) {
    char ch = Lexer_read_char(lex);
    lex->pos += 1;
    Loc_next(&lex->loc, ch);
    return ch;
}

size_t Lexer_save(Lexer *lex) {
    return lex->pos;
}

void Lexer_restore(Lexer *lex, size_t pos) {
    lex->pos = pos;
}

Token Lexer_next(Lexer *lex) {
    while(lex->pos <= lex->source.count) {
        size_t begin = lex->pos;
        char ch = Lexer_advance(lex);
        Loc loc = lex->loc;

        switch (ch) {
            case 0:
                return (Token) {
                    .kind = TOKEN_EOF,
                    .loc = loc,
                    .source = sv_from_cstr("")
                };
            case ',':
                return (Token) {
                    .kind = TOKEN_COMMA,
                    .loc = loc,
                };
            case ';':
                return (Token) {
                    .kind = TOKEN_SEMICOLON,
                    .loc = loc,
                };
            case '(':
                return (Token) {
                    .kind = TOKEN_OPEN_PAREN,
                    .loc = loc,
                };
            case ')':
                return (Token) {
                    .kind = TOKEN_CLOSE_PAREN,
                    .loc = loc,
                };
            case '{':
                return (Token) {
                    .kind = TOKEN_OPEN_BRACE,
                    .loc = loc,
                };
            case '}':
                return (Token) {
                    .kind = TOKEN_CLOSE_BRACE,
                    .loc = loc,
                };
            case '"': {
                for(;;) {
                    char ch = Lexer_read_char(lex);
                    if(ch == '"') {
                        Lexer_advance(lex);
                        break;
                    } else if(ch == 0) {
                        return (Token){
                            .kind = TOKEN_UNTERMINATED_STRING_LITERAL,
                            .loc = loc,
                            .source = sv_from_parts(lex->source.items + begin, lex->pos - begin),
                        };
                    } else {
                        Lexer_advance(lex);
                    }
                }

                return (Token){
                    .kind = TOKEN_STRING_LITERAL,
                    .loc = loc,
                    .source = sv_from_parts(lex->source.items + begin, lex->pos - begin),
                };
            } break;
            default: {
                if(isalpha(ch)) {
                    for(;;) {
                        char ch = Lexer_read_char(lex);
                        if(isalpha(ch) || ch == '_' || isalnum(ch)) Lexer_advance(lex);
                        else break;
                    }
                    String_View source = sv_from_parts(lex->source.items + begin, lex->pos - begin);
                    TokenKind kind = TOKEN_IDENTIFIER;
                    if(sv_eq(source, sv_from_cstr("import"))) {
                        TokenKind kind = TOKEN_IMPORT;
                    }
                    else if(sv_eq(source, sv_from_cstr("fn"))) {
                        TokenKind kind = TOKEN_FN;
                    }
                    return (Token){
                        .kind = kind,
                        .loc = loc,
                        .source = source,
                    };
                }
                if(isspace(ch)) continue;
                return (Token) {
                    .kind = TOKEN_UNEXPECTED_CHARACTER,
                    .loc = loc,
                    .source = sv_from_parts(lex->source.items + begin, lex->pos - begin),
                };
            } break;
        }
    }

    return (Token) {
        .kind = TOKEN_EOF,
        .loc = lex->loc,
        .source = sv_from_cstr("")
    };
}

bool compile_program(const char* input_path) {
    Lexer lex = {0};
    try(Lexer_init(&lex, input_path));
    for(;;) {
        Token tk = Lexer_next(&lex);
        if(tk.kind == TOKEN_EOF) break;
        // printf("%.*s\n", (int)tk.source_len, tk.source);
        switch(tk.kind) {
            case TOKEN_IMPORT: {
                TODO("TOKEN_IMPORT");
            } break;
            case TOKEN_FN: {
                TODO("TOKEN_FN");
            } break;
            case TOKEN_UNEXPECTED_CHARACTER:
            default: {
                printf(SV_Fmt"\n", SV_Arg(tk.source));
                TODO("TOKEN_UNEXPECTED_CHARACTER");
            } break;
        }
    }

    Lexer_deinit(&lex);
    return true;
}
