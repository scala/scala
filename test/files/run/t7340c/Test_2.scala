import scala.reflect.runtime.universe._
import scala.language.experimental.macros

class C1 { def foo: Unit = () }
class D1 extends C1 { override def foo: Unit = macro Macros.impl }

class C2 { def foo: Unit = macro Macros.impl }
class D2 extends C2 { override def foo: Unit = macro Macros.impl }

class X { def foo: Unit = macro Macros.impl }
class Y { def foo: Unit = () }

object Test extends App {
  def test1(header: String, fn: List[Type] => Any): Unit = {
    println(s"===== $header =====")
    println(fn(List(typeOf[AnyRef { def foo: Unit }], typeOf[X])))
    println(fn(List(typeOf[X], typeOf[AnyRef { def foo: Unit }])))
    println(fn(List(typeOf[AnyRef { def foo: Unit }], typeOf[Y])))
    println(fn(List(typeOf[Y], typeOf[AnyRef { def foo: Unit }])))
    println(fn(List(typeOf[X], typeOf[Y])))
    println(fn(List(typeOf[Y], typeOf[X])))
    println(fn(List(typeOf[C1], typeOf[D1])))
    println(fn(List(typeOf[D1], typeOf[C1])))
    println(fn(List(typeOf[C2], typeOf[D2])))
    println(fn(List(typeOf[D2], typeOf[C2])))
  }

  def test2(): Unit = {
    def test(tp: Type) = println(tp.typeSymbol.asInstanceOf[scala.reflect.internal.Symbols#Symbol].classBound)
    println(s"===== classBound =====")
    test(typeOf[C1])
    test(typeOf[D1])
    test(typeOf[C2])
    test(typeOf[D2])
    test(typeOf[X])
    test(typeOf[Y])
  }

  test1("lubs", lub _)
  test1("glbs", glb _)
  test2()
}