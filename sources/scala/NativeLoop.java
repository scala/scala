/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala;


public class NativeLoop {

    /** @meta method [?A] (def scala.Boolean, def ?A) scala.Unit;
     */
    public static void loopWhile(scala.Function0 cond, scala.Function0 body) {
        while (((scala.Boolean)cond.apply()).asBoolean()) {
            body.apply();
        }
    }
}
