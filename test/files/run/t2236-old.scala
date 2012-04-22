class T[A](implicit val m:Manifest[A])
class Foo
class Bar extends T[Foo]
object Test extends App {
  new Bar
}

object EvidenceTest {
  trait E[T]
  trait A[T] { implicit val e: E[T] = null }
  class B[T : E] extends A[T] { override val e = null }

  def f[T] {
    implicit val e: E[T] = null
    new B[T]{}
  }
}
