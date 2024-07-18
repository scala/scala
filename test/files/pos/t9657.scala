//> using options -Werror
sealed trait PowerSource
case object Petrol extends PowerSource
case object Pedal  extends PowerSource

sealed abstract class Vehicle { type A <: PowerSource }
case object Bicycle       extends Vehicle { type A = Pedal.type  }
case class Bus(fuel: Int) extends Vehicle { type A = Petrol.type }
case class Car(fuel: Int) extends Vehicle { type A = Petrol.type }

object Test {
  def refuel[P <: Petrol.type](vehicle: Vehicle { type A = P }): Vehicle = vehicle match {
    case Car(_) => Car(100)
    case Bus(_) => Bus(100) // was: "unreachable code" warning
  }

  def main(args: Array[String]): Unit = {
    println(refuel(Car(100)))
    println(refuel(Bus(5)))
  }
}
