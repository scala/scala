import scala.language.implicitConversions

object Test {
  abstract class Unit
  object NoUnit extends Unit
  object Hour extends Unit { override def toString = "Hour" }

  case class Measure(scalar: Double, unit: Unit) {
    def *(newUnit: Unit) = Measure(scalar, newUnit)
  }

  implicit def double2Measure(scalar: Double) =
    Measure(scalar, NoUnit)


  def main(args: Array[String]): scala.Unit = {
    Console.println("3.0 * Hour = " + (3.0 * Hour))
  }
}
