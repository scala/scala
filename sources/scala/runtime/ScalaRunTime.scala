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

  trait Try[a] {
    def Catch[b >: a](handler: PartialFunction[Throwable, b]): b;
    def Finally(handler: Unit): a;
  }

  def Try[a](block: => a): Try[a] = new Try[a] with Runnable {
    var result: a = _;
    var exception: Throwable = RunTime.tryCatch(this);

    def run(): Unit = result = block;

    def Catch[b >: a](handler: PartialFunction[Throwable, b]): b =
      if (exception == null)
	result.asInstanceOf$erased[b]
      // !!! else if (exception is LocalReturn)
      // !!!   // ...
      else if (handler isDefinedAt exception)
	handler(exception)
      else
	throw exception;

    def Finally(handler: Unit): a =
      if (exception == null)
        result.asInstanceOf$erased[a]
      else
        throw exception;
  }

}
