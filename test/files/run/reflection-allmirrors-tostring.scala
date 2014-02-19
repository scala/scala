import scala.language.higherKinds
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
  println(im.reflectField(typeOf[C].member(TermName("f1")).asTerm))
  println(im.reflectField(typeOf[C].member(TermName("f2")).asTerm))
  println(im.reflectMethod(typeOf[C].member(TermName("m1")).asMethod))
  println(im.reflectMethod(typeOf[C].member(TermName("m2")).asMethod))
  println(im.reflectMethod(typeOf[C].member(TermName("m3")).asMethod))
  println(im.reflectMethod(typeOf[C].member(TermName("m4")).asMethod))
  println(im.reflectMethod(typeOf[C].member(TermName("m5")).asMethod))
  println(im.reflectClass(typeOf[C].member(TypeName("C")).asClass))
  println(im.reflectModule(typeOf[C].member(TermName("M")).asModule))

  val c = cm.staticClass("C")
  val cc = typeOf[C].member(TypeName("C")).asClass
  println(cm.reflectClass(c).reflectConstructor(c.info.member(termNames.CONSTRUCTOR).asMethod))
  println(im.reflectClass(cc).reflectConstructor(cc.info.member(termNames.CONSTRUCTOR).asMethod))
}
