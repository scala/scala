/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.collection.immutable;

class Order[t](less:(t,t) => Boolean,equal:(t,t) => Boolean) {
    def lt (e1:t, e2:t) = less(e1,e2);
    def leq (e1:t, e2:t) = less(e1,e2) || equal(e1,e2);
    def gt (e1:t, e2:t) = less(e2,e1);
    def geq (e1:t, e2:t) = leq(e2,e1);
}
