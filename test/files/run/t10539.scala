//> using options -Xdev
//
class A {
  def ==(a: A) = "LOL"
}
case class B(a: A)

object Test extends App {
  B(new A) == B(new A)
}

// was: java.lang.ClassCastException: class java.lang.String cannot be cast to class java.lang.Boolean
