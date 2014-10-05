@scala.annotation.implicitNotFound("Could not find implicit for ${T} or ${U}") trait X[T, U]

object Test {
  type Z[U] = X[Int, U]
  implicitly[X[Int, String]]
  implicitly[Z[String]]
}
