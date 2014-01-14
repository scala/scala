import scala.reflect.macros.blackbox.Context
import language.experimental.macros

trait T { def t(): Unit }
trait A { def t(): Unit = () }

object Macro { def t(c: Context)(): c.Expr[Unit] = c.universe.reify(()) }
trait C extends T { self: A => override def t(): Unit = macro Macro.t }
