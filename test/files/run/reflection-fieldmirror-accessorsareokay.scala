import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}

object Test extends App {
  class A {
    var x: Int = 42
  }

  val a = new A

  val im: InstanceMirror = cm.reflect(a)
  val cs = im.symbol

  def test(f: Symbol) = {
    try {
      val fm: FieldMirror = im.reflectField(f.asTerm)
      println(fm.symbol.isVar)
      println(fm.get)
      fm.set(2)
      println(fm.get)
    } catch {
      case ex: Throwable =>
        println(ex.getMessage)
    }
  }

  test(cs.typeSignature.declaration(newTermName("x")).asTerm)
  test(cs.typeSignature.declaration(newTermName("x_$eq")).asTerm)
}
