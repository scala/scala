import scala.language.experimental.macros
import scala.reflect.macros.whitebox
object A {
  def f: Unit = macro f_impl
  implicit def f_impl(c: whitebox.Context): c.Expr[Unit] =
    throw new OutOfMemoryError("OOM") with scala.util.control.NoStackTrace
}
