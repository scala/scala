/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.partest.nest

@deprecated("JDK 17 deprecates SecurityManager", since="2.13.7")
object TrapExit {

  private class TrapExitThrowable(val status: Int) extends Throwable {
    override def getMessage: String = throw this
    override def getCause: Throwable = throw this
  }

  def apply[A](action: () => A): Either[(Int, Throwable), A] = {
    val saved = System.getSecurityManager
    System.setSecurityManager(new DelegatingSecurityManager(saved) {
      override def checkExit(status: Int): Unit = throw new TrapExitThrowable(status)
    })
    try {
      Right(action())
    } catch {
      case te: TrapExitThrowable =>
        Left((te.status, te))
    } finally {
      System.setSecurityManager(saved)
    }
  }
}
