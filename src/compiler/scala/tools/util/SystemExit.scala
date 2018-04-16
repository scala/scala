package scala.tools.util

import scala.util.control.ControlThrowable

/** This class exists to replace existing calls to `System.exit` or `sys.exit`
  * in the compiler. It is recommended to avoid using this class where possible,
  * and instead use a design that does not require terminating the JVM.
  *
  * @param code the exit code
  */
final case class SystemExit(code: Int) extends Throwable(s"exit code $code") with ControlThrowable
