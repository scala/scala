object Dist1 extends Enumeration { val Mile, Foot, Inch = Value }

object Dist2 extends Enumeration { val Kilometer, Millimeter, Parsec = Value }

object Test extends App {
  println(Dist1.Mile == Dist2.Kilometer)
}
