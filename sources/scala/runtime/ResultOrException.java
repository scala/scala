/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.runtime;

/** @meta class [?A] extends scala.Object;
 */
public class ResultOrException {

    /** @meta field ?A;
     */
    public Object result;

    public Throwable exc;

    /** @meta constr(?A, java.lang.Throwable);
     */
    ResultOrException(Object result, Throwable exc) {
	this.result = result;
	this.exc = exc;
    }


    /** @meta method [?A] (def ?A) scala.runtime.ResultOrException[?A];
     */
    public static ResultOrException tryBlock(scala.Function0 block) {
	try {
	    return new ResultOrException(block.apply(), null);
	} catch (Throwable ex) {
	    return new ResultOrException(null, ex);
	}
    }
}

