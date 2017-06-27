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
  // partest still uses the old name.  once we re-in-source partest we
  // can get rid of this, but in the meantime, we'd rather not branch
  // partest just because one method got renamed
  def unwrap(x: Throwable): Throwable = rootCause(x)
}
