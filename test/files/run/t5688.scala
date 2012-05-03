object Test extends App {
  trait T

  trait TA
  trait TB

  class A extends T with TA
  class B extends T with TB
  class AB extends T with TA with TB
  // Matching on _: TA with TB

  val li: Vector[T] = Vector(new A, new B, new AB)

  val matched = (for (l <- li) yield {
    l match {
      case _: TA with TB => "tab"
      case _: TA => "ta"
      case _: TB => "tb"
    }
  })

  println(matched)
}