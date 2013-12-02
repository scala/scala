package p

trait NRoot[A]

object FastComplex {
  final def sqrt(x: Double): Double = Math.sqrt(x)
  final def sqrt[A](a: A)(implicit ev: NRoot[A]): A = ???

  object Inner {
    import java.lang.Math.sqrt

    // wrong message:
    // error: reference to sqrt is ambiguous;
    //        it is both defined in object FastComplex and imported subsequently by
    sqrt(0d)
  }
}
