class A[T <: A[T]] {

}

object Test {
  val x: A[X] forSome { type X } = null
}
