/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac.ast.parser;

import scalac.util.Name;

/** A class for representing a token's data.
 *
 *  @author     Matthias Zenger
 *  @version    1.0
 */
public class TokenData implements Tokens {

    /** the next token
     */
    public int token = EMPTY;

    /** the token's position. pos = line << Position.LINESHIFT + col
     */
    public int pos = 0;

    /** the name of an identifier or token
     */
    public Name name;

    /** the value of a number
     */
    public long intVal;
    public double floatVal;

    public void copyFrom(TokenData td) {
        this.token = td.token;
        this.pos = td.pos;
        this.name = td.name;
        this.intVal = td.intVal;
        this.floatVal = td.floatVal;
    }
}
