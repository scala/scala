/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

import scalac.util.Name;

package scala.tools.scalac.ast.parser {

/** A class for representing a token's data.
 *
 *  @author     Matthias Zenger
 *  @version    1.0
 */
class TokenData {

  import Tokens._;

  /** the next token
  */
  var token: int = EMPTY;

  /** the token's position. pos = line << Position.LINESHIFT + col
  */
  var pos: int = 0;

  /** the name of an identifier or token
  */
  var name: Name = null;

  /** the base of a number
  */
  var base: int = 0;

  def copyFrom(td: TokenData) = {
    this.token = td.token;
    this.pos = td.pos;
    this.name = td.name;
    this.base = td.base;
  }
}
}
