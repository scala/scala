package tastytest

object Logarithms {

  opaque type Logarithm = Double

  object Logarithm {

    // These are the two ways to lift to the Logarithm type

    private[Logarithms] def apply(d: Double): Logarithm = math.log(d)

    def of(d: Double): Option[Logarithm] =
      if (d > 0.0) Some(math.log(d)) else None
  }

  // implicit define cross compatible public APIs for opaque types
  final implicit class LogarithmOps(val logarithm: Logarithm) extends AnyVal {
    def toDouble: Double = math.exp(logarithm)
    def + (other: Logarithm): Logarithm = Logarithm(math.exp(logarithm) + math.exp(other))
    def * (other: Logarithm): Logarithm = logarithm + other
  }

}
