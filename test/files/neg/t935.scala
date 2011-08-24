import annotation.Annotation

object Test3 {
  class E[T >: Nothing <: String](s: T) extends Annotation
  class B
  // val a = new E[B](new B)
  @E[B](new B) val b = "hi"
}

object Test4 {
  class E[T <: String](s: T) extends Annotation
  class B
  val b: String @E[B](new B) = "hi"
}
