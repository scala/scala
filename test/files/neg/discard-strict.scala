
// scalac: -Ywarn-value-discard-strict -Xfatal-warnings

// no warnings under vanilla -Ywarn-value-discard
trait T[A] {
  val g: collection.mutable.Growable[A]

  def f(a: A): Unit = g += a

  def unit: Unit = {
    42: Unit

    17: Unit
  }
}
