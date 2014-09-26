
trait Test {
  type R = PartialFunction[Any, Unit]

  val x: R = { case "" => }
  val y: R = { case "" => }

  val z: R = x orElse y
  val zz   = x orElse y
}

// Any pops up in middle of beta reduction
trait Retest {
  type S[A]   = PartialFunction[String, Unit]
  type U[A,B] = S[Any]
  type R      = U[Nothing, Unit]
  type X[A]   = U[Nothing, Unit]

  val x: R = { case "" => }
  val y: R = { case "" => }

  val z: R = x orElse y
  val zz   = x orElse y

  val zx: X[Any] = x orElse y
}

/*
trait Widening {
  type S[A]   = PartialFunction[String, Unit]
  type R      = Y.type

  object Y extends S[Any] {
    override def apply(s: String) = ()
    override def isDefinedAt(s: String) = true
  }

  val x: R = Y
  val y: R = Y

  //val z: R = x orElse y
  val zz   = x orElse y
}
*/
