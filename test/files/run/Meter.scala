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

  {
  val x: Meter = new Meter(1)
  val a: Object = x.asInstanceOf[Object]
  val y: Meter = a.asInstanceOf[Meter]

  val u: Double = 1
  val b: Object = u.asInstanceOf[Object]
  val v: Double = b.asInstanceOf[Double]
  }

  val x = new Meter(1)
  val y = x
  println((x + x) / x)
  println((x + x) / 0.5)
  println((x < x).toString)
  println("x.isInstanceOf[Meter]: "+x.isInstanceOf[Meter])


  println("x.hashCode: "+x.hashCode)
  println("x == 1: "+(x == 1))
  println("x == y: "+(x == y))
  assert(x.hashCode == (1.0).hashCode)
  
  val a: Any = x
  val b: Any = y
  println("a == b: "+(a == b))

  val arr = Array(x, y + x)
}
