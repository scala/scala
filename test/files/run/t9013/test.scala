import scala.annotation.varargs

abstract class VarargAbstractClass[T] {
  @varargs
  def x(els: String*): Int

  @varargs
  def y(els: String*): Int

  @varargs
  def z(els: T*): Int
}
class ClassImplementsClass extends VarargAbstractClass[String] {

  override def x(els: String*): Int = els.length
  override def y(els: String*): Int = els.length
  override def z(els: String*): Int = els.length
}
