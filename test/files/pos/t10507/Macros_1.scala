import scala.language.experimental.macros
import scala.reflect.macros.blackbox

object Macros_1 {
  def seq: List[Int] = macro impl
  def impl(c: blackbox.Context): c.Tree = {
    import c.universe._
    q"""{
      class A(val i: Int) { def this() = this(0) }
      List(new A, new A(1)).map(_.i)
    }"""
  }
}
