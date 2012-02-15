import collection.generic.RowFactory

class Meter(val unbox: Double) extends AnyVal with Boxed[Double] with Printable {
   def + (other: Meter): Meter = 
     new Meter(this.unbox + other.unbox)
   def / (other: Meter): Double = this.unbox / other.unbox
   def / (factor: Double): Meter = new Meter(this.unbox / factor)
   def < (other: Meter): Boolean = this.unbox < other.unbox
   override def toString: String = unbox.toString+"m"
}
object Meter extends RowFactory[Double, Meter] {
  def apply(x: Double): Meter = new Meter(x)
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

  { println("testing native arrays")
    val arr = Array(x, y + x)
    println(arr.deep)
    def foo[T <: Printable](x: Array[T]) {
      for (i <- 0 until x.length) { x(i).print; println(" "+x(i)) }
    }
    val m = arr(0)
    println(m)
    foo(arr)
  }

  { println("testing wrapped arrays")
    val arr = Meter.Row(x, y + x)
    println(arr)
    def foo(x: Meter.Row) {
      for (i <- 0 until x.length) { x(i).print; println(" "+x(i)) }
    }
    val m = arr(0)
    println(m)
    foo(arr)
  }

}
