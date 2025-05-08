/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */

enum {
  STRING_ERROR_NONE,
  STRING_ERROR_NULL_CHAR,
  STRING_ERROR_TOO_LONG
};
int string_error;
void add_string_char(char ch);

int comment_nesting_level;

%}

%option noyywrap
%x STRING COMMENT

/*
 * Define names for regular expressions here.
 */

%%

 /*
  *  Maintain line number count
  */
<*>\n {
  ++curr_lineno;
  REJECT;
}

 /*
  *  Nested comments
  */
"(*" {
  BEGIN(COMMENT);
  comment_nesting_level = 0;
}
"*)" {
  cool_yylval.error_msg = "Unmatched *)";
  return ERROR;
}
<COMMENT>{
  "(*" {
    ++comment_nesting_level;
  }
  "*)" {
    if (comment_nesting_level == 0) {
      BEGIN(INITIAL);
    } else {
      --comment_nesting_level;
    }
  }
  <<EOF>> {
    BEGIN(INITIAL);
    cool_yylval.error_msg = "EOF in comment";
    return ERROR;
  }
  .|\n
}

 /*
  *  One line comments
  */
--.*$

 /*
  * Integers
  */
[0-9]+ {
  cool_yylval.symbol = inttable.add_string(yytext);
  return INT_CONST;
}

 /*
  *  The multiple-character operators.
  */
=> return DARROW;
"<-" return ASSIGN;
"<=" return LE;

 /*
  *  The single-character tokens.
  */
[;,:.@{}()+\-*/~<=] return yytext[0];

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */

(?i:CLASS) return CLASS;
(?i:ELSE) return ELSE;
(?i:FI) return FI;
(?i:IF) return IF;
(?i:IN) return IN;
(?i:INHERITS) return INHERITS;
(?i:ISVOID) return ISVOID;
(?i:LET) return LET;
(?i:LOOP) return LOOP;
(?i:POOL) return POOL;
(?i:THEN) return THEN;
(?i:WHILE) return WHILE;
(?i:CASE) return CASE;
(?i:ESAC) return ESAC;
(?i:NEW) return NEW;
(?i:OF) return OF;
(?i:NOT) return NOT;

f(?i:alse)|t(?i:rue) {
  cool_yylval.boolean = yytext[0] == 't';
  return BOOL_CONST;
}

 /*
  * Type identifiers
  */
[A-Z][A-Za-z0-9_]* {
  cool_yylval.symbol = idtable.add_string(yytext);
  return TYPEID;
}

 /*
  * Object identifiers
  */
[a-z][A-Za-z0-9_]* {
  cool_yylval.symbol = idtable.add_string(yytext);
  return OBJECTID;
}

 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */
\" {
  string_buf_ptr = string_buf;
  string_error = STRING_ERROR_NONE;
  BEGIN(STRING);
}
<STRING>{
  \" {
    BEGIN(INITIAL);
    switch (string_error) {
      case STRING_ERROR_NONE:
        *string_buf_ptr = 0;
        cool_yylval.symbol = stringtable.add_string(string_buf);
        return STR_CONST;
      case STRING_ERROR_NULL_CHAR:
        cool_yylval.error_msg = "String contains null character";
        return ERROR;
      case STRING_ERROR_TOO_LONG:
        cool_yylval.error_msg = "String constant too long";
        return ERROR;
    }
  }
  \\n add_string_char('\n');
  \\t add_string_char('\t');
  \\b add_string_char('\b');
  \\f add_string_char('\f');
  \\(.|\n) add_string_char(yytext[1]);
  \0 {
    if (string_error == STRING_ERROR_NONE) {
      string_error = STRING_ERROR_NULL_CHAR;
    }
  }
  \n {
    BEGIN(INITIAL);
    cool_yylval.error_msg = "Unterminated string constant";
    return ERROR;
  }
  <<EOF>> {
    BEGIN(INITIAL);
    cool_yylval.error_msg = "EOF in string constant";
    return ERROR;
  }
  . add_string_char(yytext[0]);
}

 /* 
  * Whitespace
  */

[ \n\f\r\t\v] ;

 /* 
  * Invalid characters in input
  */
. {
  cool_yylval.error_msg = strndup(yytext, 1);
  return ERROR;
}

%%

void add_string_char(char ch) {
  if (string_error == STRING_ERROR_NONE) {
    *string_buf_ptr = ch;
    ++string_buf_ptr;
    if (string_buf_ptr == string_buf + MAX_STR_CONST) {
      string_error = STRING_ERROR_TOO_LONG;
    }
  }
}
