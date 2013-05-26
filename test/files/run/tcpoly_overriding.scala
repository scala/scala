
import scala.language.{ higherKinds }

abstract class A[t[x]] {
  def b: t[Int]
}

class B extends A[List] {
    // underlying functionality being tested is overriding, but bugs manifest itself during erasure
    // erasure should generate two methods: one that returns an Object (to implement the method in A)
    // one that is as close as possible to the original method and thus returns a List
    // the problem only manifests itself here -- but it's really a problem with overriding
    // the link between this method and the method in A isn't seen
  def b: List[Int] = List(1)
}

object Test extends App {
  Console.println((new B).b(0))
}
