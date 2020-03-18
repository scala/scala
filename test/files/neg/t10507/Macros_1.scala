import scala.language.experimental.macros
import scala.reflect.macros.blackbox

object Macros_1 {
  def seq: List[Int] = macro impl
  def impl(c: blackbox.Context): c.Tree = {
    import c.universe._
    q"""{
      class A(val l: Long) { def this() = this(0); def this(i: Int) = this(i: Long) }
      List(new A, new A(1), new A(2L)).map(_.l)
    }"""
  }
}
