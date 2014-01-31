import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}

object Test extends App {
  class A {
    private[this] var x: Int = 42
  }

  val a = new A

  val im: InstanceMirror = cm.reflect(a)
  val cs = im.symbol
  val f = cs.info.decl(TermName("x")).asTerm
  val fm: FieldMirror = im.reflectField(f)
  println(fm.symbol.isVar)
  println(fm.get)
  fm.set(2)
  println(fm.get)
}
