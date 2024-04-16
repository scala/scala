//> using options -Xfatal-warnings
object D {
  sealed trait T
  final case class C(i: Int) extends T
  final case class D(i: Int) extends T

  object NoCoverage {
    def unapply(t: T): Option[Int] = None
  }

  def extractorOnly(t: T): Unit = t match {
    case NoCoverage(_) =>
  }

  def extractorAndClass(t: T): Unit = t match {
    case NoCoverage(_) =>
    case C(_) =>
  }
}
