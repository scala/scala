package p

trait NRoot[A]

object `package` {
  final def sqrt(x: Double): Double = Math.sqrt(x)
  final def sqrt[A](a: A)(implicit ev: NRoot[A]): A = ???
}

object FastComplex {
  import java.lang.Math.sqrt

  sqrt(0d)
}
