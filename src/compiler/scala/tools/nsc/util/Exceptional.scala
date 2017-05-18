package scala.tools.nsc
package util

import java.util.concurrent.ExecutionException
import java.lang.reflect.{ InvocationTargetException, UndeclaredThrowableException }

object Exceptional {
  def rootCause(x: Throwable): Throwable = x match {
    case  _: InvocationTargetException |
          _: ExceptionInInitializerError |
          _: UndeclaredThrowableException |
          _: ExecutionException
            if x.getCause != null =>
              rootCause(x.getCause)

    case _ => x
  }
}
