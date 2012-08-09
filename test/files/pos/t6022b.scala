trait A
trait B
trait C
trait AB extends B with A

// two types are mutually exclusive if there is no equality symbol whose constant implies both
object Test extends App {
  def foo(x: Any) = x match {
    case _ : C  => println("C")
    case _ : AB => println("AB")
    case _ : (A with B) => println("AB'")
    case _ : B  => println("B")
    case _ : A  => println("A")
  }

  foo(new A {})
  foo(new B {})
  foo(new AB{})
  foo(new C {})
}
