class C {
  val foo = 1
  def bar = 2
  class C { override def toString = "CC" }
  object O { override def toString = "CO" }
}

class D {
  val foo = 3
  def bar = 4
  class C { override def toString = "DC" }
  object O { override def toString = "DO" }
}

object Test extends App {
  import scala.reflect.runtime.universe._
  import scala.reflect.runtime.{currentMirror => cm}
  val im = cm.reflect(new C)

  def test(tpe: Type): Unit = {
    def failsafe(action: => Any): Any = try action catch { case ex: Throwable => ex.toString }
    println("field: " + failsafe(im.reflectField(tpe.member(newTermName("foo")).asTermSymbol).get))
    println("method: " + failsafe(im.reflectMethod(tpe.member(newTermName("bar")).asMethodSymbol)()))
    println("class: " + failsafe(im.reflectClass(tpe.member(newTypeName("C")).asClassSymbol).reflectConstructor(typeOf[C].member(newTypeName("C")).asClassSymbol.typeSignature.member(newTermName("<init>")).asMethodSymbol)()))
    println("object: " + failsafe(im.reflectModule(tpe.member(newTermName("O")).asModuleSymbol).instance))
  }

  test(typeOf[C])
  test(typeOf[D])
}