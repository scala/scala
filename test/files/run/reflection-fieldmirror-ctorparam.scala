import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}

object Test extends App {
  class A(x: Int) {
    private[this] var xx = x
  }

  val a = new A(42)

  val im: InstanceMirror = cm.reflect(a)
  val cs = im.symbol
  val f = cs.info.decl(TermName("x")).asTerm
  try {
    val fm: FieldMirror = im.reflectField(f)
    println(fm.get)
  } catch {
    case ex: Throwable =>
      println(s"${ex.getClass}: ${ex.getMessage}")
  }
}
