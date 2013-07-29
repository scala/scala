import scala.reflect.macros.Context
import language.experimental.macros

trait T { def t(): Unit }
abstract class A extends T { override def t(): Unit = () }

object Macro { def t(c: Context)(): c.Expr[Unit] = c.universe.reify(()) }
class C extends A { override def t(): Unit = macro Macro.t }
