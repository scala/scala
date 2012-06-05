import scala.reflect.runtime.universe._

object Test extends App {
  class D[T: TypeTag] {
    class C[U: TypeTag] {
      def foo[V] = macro Impls.foo[List[T], U, V]
      foo[Boolean]
    }
  }

  val outer1 = new D[Int]
  new outer1.C[String]
}