package tastytest

import java.lang.StrictMath

object Logarithms {

  opaque type Logarithm = Double

  object Logarithm {

    // These are the two ways to lift to the Logarithm type

    private[Logarithms] def apply(d: Double): Logarithm = StrictMath.log10(d)

    def of(d: Double): Option[Logarithm] =
      if (d > 0.0) Some(StrictMath.log10(d)) else None
  }

  // implicit define cross compatible public APIs for opaque types
  final implicit class LogarithmOps(val logarithm: Logarithm) extends AnyVal {
    def toDouble: Double = StrictMath.pow(10.0, logarithm)
    def + (other: Logarithm): Logarithm = Logarithm(StrictMath.pow(10.0, logarithm) + StrictMath.pow(10.0, other))
    def * (other: Logarithm): Logarithm = logarithm + other
  }

}
