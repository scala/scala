class C {
  val foo = 1
  def bar = 2
  class C { override def toString = "CC" }
  object O { override def toString = "CO" }
  override def toString = "an instance of class C"
}

class D {
  val foo = 3
  def bar = 4
  class C { override def toString = "DC" }
  object O { override def toString = "DO" }
  override def toString = "an instance of class D"
}

object Test extends App {
  import scala.reflect.runtime.universe._
  import scala.reflect.runtime.{currentMirror => cm}
  val im = cm.reflect(new C)

  def test(tpe: Type): Unit = {
    def failsafe(action: => Any): Any = try action catch { case ex: Throwable => ex.toString }
    println("field: " + failsafe(im.reflectField(tpe.member(newTermName("foo")).asTerm).get))
    println("method: " + failsafe(im.reflectMethod(tpe.member(newTermName("bar")).asMethod)()))
    println("constructor #1: " + failsafe(cm.reflectClass(im.symbol).reflectConstructor(tpe.member(newTermName("bar")).asMethod)()))
    println("constructor #2: " + failsafe(cm.reflectClass(im.symbol).reflectConstructor(tpe.member(newTermName("<init>")).asMethod)()))
    println("class: " + failsafe(im.reflectClass(tpe.member(newTypeName("C")).asClass).reflectConstructor(typeOf[C].member(newTypeName("C")).asClass.typeSignature.member(newTermName("<init>")).asMethod)()))
    println("object: " + failsafe(im.reflectModule(tpe.member(newTermName("O")).asModule).instance))
  }

  test(typeOf[C])
  test(typeOf[D])
}