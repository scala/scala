/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac.ast.parser;

public interface Tokens {
   byte EMPTY = -3,
        UNDEF = -2,
        ERROR = -1,
        EOF = 0,

        /* literals */
        CHARLIT = 1,
        INTLIT = 2,
        LONGLIT = 3,
        FLOATLIT = 4,
        DOUBLELIT = 5,
        STRINGLIT = 6,
        SYMBOLLIT = 7,

        /* identifier */
        IDENTIFIER = 10,

        /* keywords */
        IF = 20,
        FOR = 21,
        ELSE = 22,
        THIS = 23,
        NULL = 24,
        NEW = 25,
        WITH = 26,
        SUPER = 27,
        CASE = 28,
        CASECLASS = 29,
        CASEOBJECT = 30,
        VAL = 31,
        ABSTRACT = 32,
        FINAL = 33,
        PRIVATE = 34,
        PROTECTED = 35,
        OVERRIDE = 36,
        VAR = 37,
        DEF = 38,
        TYPE = 39,
        EXTENDS = 40,
        TRUE = 41,
        FALSE = 42,
        OBJECT = 43,
        CLASS = 44,

        IMPORT = 46,
        PACKAGE = 47,
        YIELD = 48,
        DO = 49,
        TRAIT = 50,
        SEALED = 51,
        THROW = 52,
        TRY = 53,
        CATCH = 54,
        FINALLY = 55,
        WHILE = 56,
        RETURN = 57,

        /* special symbols */
        COMMA = 61,
        SEMI = 62,
        DOT = 63,
        USCORE = 64,
        COLON = 65,
        EQUALS = 66,
        LARROW = 67,
        ARROW = 68,
        SUBTYPE = 69,
        SUPERTYPE = 70,
        HASH = 71,
        AT = 72,

        /* parenthesis */
        LPAREN = 90,
        RPAREN = 91,
        LBRACKET = 92,
        RBRACKET = 93,
        LBRACE = 94,
        RBRACE = 95;
}


