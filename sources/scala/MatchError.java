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


/** This class implements errors which are thrown whenever an
 *  object doesn't match any pattern of a pattern matching
 *  expression.
 *
 *  @author  Matthias Zenger
 *  @version 1.1, 05/03/2004
 */
public final class MatchError extends Error {

    /** @meta constr (java.lang.String, scala.Int);
     */
    private MatchError(java.lang.String source, int line) {
        super(" in '" + source + "' at line " + line);
    }

    /** @meta constr (java.lang.String, scala.Int, java.lang.String);
     */
    private MatchError(java.lang.String source, int line, String obj) {
        super("for object " + obj + " in '" + source + "' at line " + line);
    }

    /** @meta method [?T](java.lang.String, scala.Int) ?T;
     */
    public static java.lang.Object fail(java.lang.String source, int line) {
        throw new MatchError(source, line);
    }

    /** @meta method [?T](java.lang.String, scala.Int, scala.Any) ?T;
     */
    public static java.lang.Object report(java.lang.String source, int line, java.lang.Object obj) {
    	try {
        	throw new MatchError(source, line, obj.toString());
        } catch (MatchError e) {
        	throw e;
        } catch (Throwable e) {
        	throw new MatchError(source, line);
        }
    }
}
