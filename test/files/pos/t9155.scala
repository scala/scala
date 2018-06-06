import scala.reflect.runtime.universe.{TypeTag, typeTag}

class A[T]
object A {
  type T = A[_]
  typeTag[T]
}

class B[T]
object B {
  type Any = B[ _ <: String]
  typeTag[B[_ <: String]]
  typeTag[B.Any]
}

class C[T]
object C {
  def f[T](implicit m: TypeTag[T]) = 0
  type CAlias = C[_]
  val x = f[CAlias]
}
