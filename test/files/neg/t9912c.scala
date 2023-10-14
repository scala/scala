class A
class B extends A

class C1 { def compareTo(a: A): Int = 0 }

trait C2[T <: A] { def compareTo(t: T): Int }

class C3 extends C1 with C2[B] { def compareTo(b: B): Int = 1 }

object Test extends App {
  println {
    (new C3).compareTo(new A)
  }
}
