import scala.reflect.macros.BlackboxContext
import language.experimental.macros

trait T { def t(): Unit }
trait A { def t(): Unit = () }

object Macro { def t(c: BlackboxContext)(): c.Expr[Unit] = c.universe.reify(()) }
trait C extends T { self: A => override def t(): Unit = macro Macro.t }
