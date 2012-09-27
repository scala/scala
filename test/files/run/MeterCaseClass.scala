package a {
  abstract class BoxingConversions[Boxed, Unboxed] {
    def box(x: Unboxed): Boxed
    def unbox(x: Boxed): Unboxed
  }

  case class Meter(underlying: Double) extends AnyVal with _root_.b.Printable {
    def + (other: Meter): Meter =
      new Meter(this.underlying + other.underlying)
    def / (other: Meter)(implicit dummy: Meter.MeterArg = null): Double = this.underlying / other.underlying
    def / (factor: Double): Meter = new Meter(this.underlying / factor)
    def < (other: Meter): Boolean = this.underlying < other.underlying
    def toFoot: Foot = new Foot(this.underlying * 0.3048)
    override def print = { Console.print(">>>"); super.print; proprint }
  }

  object Meter extends (Double => Meter) {

    private[a] trait MeterArg

    implicit val boxings = new BoxingConversions[Meter, Double] {
      def box(x: Double) = new Meter(x)
      def unbox(m: Meter) = m.underlying
    }
  }

  class Foot(val unbox: Double) extends AnyVal {
    def + (other: Foot): Foot =
      new Foot(this.unbox + other.unbox)
    override def toString = unbox.toString+"ft"
  }
  object Foot {
    implicit val boxings = new BoxingConversions[Foot, Double] {
      def box(x: Double) = new Foot(x)
      def unbox(m: Foot) = m.unbox
    }
  }

}
package b {
  trait Printable extends Any {
    def print: Unit = Console.print(this)
    protected def proprint = Console.print("<<<")
  }
}
import a._
import _root_.b._
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
  //
  // { println("testing wrapped arrays")
  //   import collection.mutable.FlatArray
  //   val arr = FlatArray(x, y + x)
  //   println(arr)
  //   def foo(x: FlatArray[Meter]) {
  //     for (i <- 0 until x.length) { x(i).print; println(" "+x(i)) }
  //   }
  //   val m = arr(0)
  //   println(m)
  //   foo(arr)
  //   val ys: Seq[Meter] = arr map (_ + new Meter(1))
  //   println(ys)
  //   val zs = arr map (_ / Meter(1))
  //   println(zs)
  //   val fs = arr map (_.toFoot)
  //   println(fs)
  // }

}
