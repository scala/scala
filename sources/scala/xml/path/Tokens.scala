package scala.xml.path ;

/** Tokens for XPath expressions
 */

class Tokens {

  final val AT         = 0;
  final val COMMA      = AT + 1;
  final val DOT        = COMMA + 1;
  final val DOTDOT     = DOT + 1;
  final val EQUALS     = DOTDOT + 1;
  final val IDENT      = EQUALS + 1;
  final val LBRACKET   = IDENT + 1;
  final val RBRACKET   = LBRACKET + 1;
  final val SLASH      = RBRACKET + 1;
  final val SLASHSLASH = SLASH + 1;
  final val STAR       = SLASHSLASH + 1;

  final val ENDTOK     = STAR + 1;


}
