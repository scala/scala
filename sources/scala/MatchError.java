/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
**                                                                      **
** $Id$
\*                                                                      */

package scala;

public class MatchError extends Error {

    /** @meta constr (java.lang.String, scala.Int);
     */
    public MatchError(java.lang.String source, int line) {
        super(" in '" + source + "' at line " + line);
    }

    /** @meta method [?T](java.lang.String, scala.Int) ?T;
     */
    public static java.lang.Object fail(java.lang.String source, int line) {
        throw new MatchError(source, line);
    }
}
