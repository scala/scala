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
        VAL = 30,
        ABSTRACT = 31,
        FINAL = 32,
        PRIVATE = 33,
        PROTECTED = 34,
        OVERRIDE = 35,
        VAR = 36,
        DEF = 37,
        TYPE = 38,
        EXTENDS = 39,
        TRUE = 40,
        FALSE = 41,
        MODULE = 43,
        CLASS = 44,
        CONSTR = 45,
        IMPORT = 46,
        PACKAGE = 47,
        AS = 48,
        IS = 49,
        YIELD = 50,
        DO = 51,
        TRAIT = 52,

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
        HASH = 70,

        /* parenthesis */
        LPAREN = 90,
        RPAREN = 91,
        LBRACKET = 92,
        RBRACKET = 93,
        LBRACE = 94,
        RBRACE = 95;
}


