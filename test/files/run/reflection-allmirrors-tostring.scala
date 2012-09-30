import scala.reflect.runtime.universe._

class C {
  val f1 = 2
  var f2 = 3

  def m1 = 4
  def m2() = 5
  def m3[T >: String <: Int]: T = ???
  def m4[A[_], B <: A[Int]](x: A[B])(implicit y: Int) = ???
  def m5(x: => Int, y: Int*): String = ???

  class C
  object M

  override def toString = "an instance of C"
}
object M

object Test extends App {
  val cm = scala.reflect.runtime.currentMirror
//  println(cm)

  println(cm.reflectClass(cm.staticClass("C")))
  println(cm.reflectModule(cm.staticModule("M")))
  println(cm.reflect(new C))

  val im = cm.reflect(new C)
  println(im.reflectField(typeOf[C].member(newTermName("f1")).asTerm))
  println(im.reflectField(typeOf[C].member(newTermName("f2")).asTerm))
  println(im.reflectMethod(typeOf[C].member(newTermName("m1")).asMethod))
  println(im.reflectMethod(typeOf[C].member(newTermName("m2")).asMethod))
  println(im.reflectMethod(typeOf[C].member(newTermName("m3")).asMethod))
  println(im.reflectMethod(typeOf[C].member(newTermName("m4")).asMethod))
  println(im.reflectMethod(typeOf[C].member(newTermName("m5")).asMethod))
  println(im.reflectClass(typeOf[C].member(newTypeName("C")).asClass))
  println(im.reflectModule(typeOf[C].member(newTermName("M")).asModule))

  val c = cm.staticClass("C")
  val cc = typeOf[C].member(newTypeName("C")).asClass
  println(cm.reflectClass(c).reflectConstructor(c.typeSignature.member(nme.CONSTRUCTOR).asMethod))
  println(im.reflectClass(cc).reflectConstructor(cc.typeSignature.member(nme.CONSTRUCTOR).asMethod))
}