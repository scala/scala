//> using options -Xsource:3
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
  def tsChild: T[String]
}
trait Child extends Parent {
  implicit val tsChild = new T[String] {}     // warn (no warn with -Xsource-features:infer-override)
  def f = new C[String]
}
