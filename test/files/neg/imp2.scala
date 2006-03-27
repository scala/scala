abstract class C {
  val f: int
}

object A extends C {
  val f = 1
}

object B extends C {
  val f = 2
}

object Test {
  val a: C = A;
  val b: C = B;
  import a._
  import b._
  val x = f
}
