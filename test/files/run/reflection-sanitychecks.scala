class C {
  val foo = 11
  def bar = 12
  val quux = 13
  def baz = 14
  class C { override def toString = "CC" }
  object O { override def toString = "CO" }
  override def toString = "an instance of class C"
}

class D extends C {
  override val foo = 21
  override def bar = 22
  override def toString = "an instance of class D"
}

class E {
  val foo = 31
  def bar = 32
  val quux = 33
  def baz = 34
  class C { override def toString = "EC" }
  object O { override def toString = "EO" }
  override def toString = "an instance of class E"
}

object Test extends App {
  import scala.reflect.runtime.universe._
  import scala.reflect.runtime.{currentMirror => cm}
  val im = cm.reflect(new D)

  def test(tpe: Type): Unit = {
    def failsafe(action: => Any): Any = try action catch { case ex: Throwable => ex.toString }
    println(s"=========members of ${tpe.typeSymbol.name} in a mirror of D=========")
    println("field #1: " + failsafe(im.reflectField(tpe.member(TermName("foo")).asTerm).get))
    println("method #1: " + failsafe(im.reflectMethod(tpe.member(TermName("bar")).asMethod)()))
    println("field #2: " + failsafe(im.reflectField(tpe.member(TermName("quux")).asTerm).get))
    println("method #2: " + failsafe(im.reflectMethod(tpe.member(TermName("baz")).asMethod)()))
    println("constructor #1: " + failsafe(cm.reflectClass(im.symbol).reflectConstructor(tpe.member(TermName("bar")).asMethod)()))
    println("constructor #2: " + failsafe(cm.reflectClass(im.symbol).reflectConstructor(tpe.member(TermName("<init>")).asMethod)()))
    println("class: " + failsafe(im.reflectClass(tpe.member(TypeName("C")).asClass).reflectConstructor(typeOf[C].member(TypeName("C")).asClass.info.member(termNames.CONSTRUCTOR).asMethod)()))
    println("object: " + failsafe(im.reflectModule(tpe.member(TermName("O")).asModule).instance))
    println()
  }

  test(typeOf[C])
  test(typeOf[D])
  test(typeOf[E])
}