/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.runtime;

object ScalaRunTime {

    class Try[a](r: scala.runtime.ResultOrException[a]) {
      def Catch[b >: a](handler: PartialFunction[Throwable, b]): b =
	if (r.exc == null)
	    r.result.asInstanceOf[b]
	else if (/*!(r.exc is NonLocalReturn) && */handler isDefinedAt r.exc)
	    handler(r.exc)
	else
	    throw r.exc;

      def Finally(handler: Unit): a =
	if (r.exc == null) r.result.asInstanceOf[a] else throw r.exc;
    }

    def Try[a](def/*!!!*/ block: a): Try[a] =
        new Try(ResultOrException.tryBlock(block));

}
