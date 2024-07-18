//> using options -Werror
trait T[A]

class C[A: T]

trait Missing {
  implicit val tsMissing = new T[String] {}   // warn val in trait
  def f = new C[String]
}
trait Local {
  def f = {
    implicit val tsLocal = new T[String] {}   // nowarn because local
    new C[String]
  }
}
trait Parent {
  def t: T[String]
}
trait Child extends Parent {
  implicit val tsChild = new T[String] {}     // warn because inferred from RHS
  def f = new C[String]
  implicit private[this] val pChild = 42      // also warn
}
class D {
  implicit val tsD = new T[String] {}         // warn val in class
  def f = new C[String]
  implicit private[this] val y = 42           // also warn
}
class X extends Missing
trait Z {
  val z = 42
}
