/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.runtime;


public class NativeMonitor  {

    /** @meta method [?A] (def ?A) ?A;
     */
    public java.lang.Object synchronised(scala.Function0 p) {
		java.lang.Object result;
		synchronized(this) {
	    	result = p.apply();
		}
		return result;
    }

    /** @meta method [?A] (scala.AnyRef, def ?A) ?A;
     */
    public static java.lang.Object synchronised(java.lang.Object any, scala.Function0 p) {
    	java.lang.Object result;
		synchronized(any) {
	    	result = p.apply();
		}
		return result;
    }
}
