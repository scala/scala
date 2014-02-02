/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package javac

object JavaTokens extends ast.parser.CommonTokens {

  def isLiteral(code: Int) =
    code >= CHARLIT && code <= STRINGLIT

  /** identifiers */
  final val IDENTIFIER = 10
  def isIdentifier(code: Int) =
    code == IDENTIFIER

  /** keywords */
  final val INSTANCEOF = 27
  final val CONST = 28

  /** modifiers */
  final val PUBLIC = 42
  final val DEFAULT = 47
  final val STATIC = 48
  final val TRANSIENT = 50
  final val VOLATILE = 51
  final val SYNCHRONIZED = 52
  final val NATIVE = 53
  final val STRICTFP = 54
  final val THROWS = 56

  /** templates */
  final val INTERFACE = 66
  final val ENUM = 67
  final val IMPLEMENTS = 69

  /** control structures */
  final val BREAK = 87
  final val CONTINUE = 88
  final val GOTO = 89
  final val SWITCH = 94
  final val ASSERT = 98

  /** special symbols */
  final val EQEQ = 140
  final val BANGEQ = 141
  final val LT = 142
  final val GT = 143
  final val LTEQ = 144
  final val GTEQ = 145
  final val BANG = 146
  final val QMARK = 147
  final val AMP = 148
  final val BAR = 149
  final val PLUS = 150
  final val MINUS = 151
  final val ASTERISK = 152
  final val SLASH = 153
  final val PERCENT = 154
  final val HAT = 155
  final val LTLT = 156
  final val GTGT = 157
  final val GTGTGT = 158
  final val AMPAMP = 159
  final val BARBAR = 160
  final val PLUSPLUS = 161
  final val MINUSMINUS = 162
  final val TILDE = 163
  final val DOTDOTDOT = 164
  final val AMPEQ = 165
  final val BAREQ = 166
  final val PLUSEQ = 167
  final val MINUSEQ = 168
  final val ASTERISKEQ = 169
  final val SLASHEQ = 170
  final val PERCENTEQ = 171
  final val HATEQ = 172
  final val LTLTEQ = 173
  final val GTGTEQ = 174
  final val GTGTGTEQ = 175

  /** primitive types */
  final val VOID = 180
  final val BOOLEAN = 181
  final val BYTE = 182
  final val SHORT = 183
  final val CHAR = 184
  final val INT = 185
  final val LONG = 186
  final val FLOAT = 187
  final val DOUBLE = 188
}
