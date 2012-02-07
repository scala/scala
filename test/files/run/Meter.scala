class Meter(val underlying: Double) extends AnyVal with Printable {
   def + (other: Meter): Meter = 
     new Meter(this.underlying + other.underlying)
   def / (other: Meter): Double = this.underlying / other.underlying
   def / (factor: Double): Meter = new Meter(this.underlying / factor)
   def < (other: Meter): Boolean = this.underlying < other.underlying
   override def toString: String = underlying.toString+"m"
}
trait Printable extends Any { def print: Unit = Console.print(this) }

object Test extends App {

  val x = new Meter(1)
  println((x + x) / x)
  println((x + x) / 0.5)
  println((x < x).toString)
  

}
