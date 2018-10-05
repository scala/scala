/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package util

import java.util.concurrent.ExecutionException
import java.lang.reflect.{ InvocationTargetException, UndeclaredThrowableException }

object Exceptional {
  def unwrap(x: Throwable): Throwable = x match {
    case  _: InvocationTargetException |
          _: ExceptionInInitializerError |
          _: UndeclaredThrowableException |
          _: ExecutionException
            if x.getCause != null =>
              unwrap(x.getCause)

    case _ => x
  }
}
