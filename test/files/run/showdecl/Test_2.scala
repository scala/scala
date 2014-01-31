import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}

object Test extends App {
  def test(sym: Symbol): Unit = {
    println(s"autoinitialized ${sym.name}: ${showDecl(sym)}")
    sym.info
    println(s"autoinitialized ${sym.name}: ${showDecl(sym)}")
  }

  Macros.foo
  println("runtime")
  test(symbolOf[D])
  test(typeOf[D].member(TermName("x")))
  test(typeOf[D].member(TermName("y")))
  test(typeOf[D].member(TermName("z")))
  test(typeOf[D].member(TermName("t")))
  test(typeOf[D].member(TypeName("W")))
  test(typeOf[D].member(TypeName("C")))
  test(typeOf[D].member(TermName("O")))
}

class C
class D extends C {
  val x = 2
  lazy val y = 3
  var z = 4
  def t[T <: Int](x: D)(y: x.W) = 5
  type W = String
  class C extends D
  object O extends C
}
