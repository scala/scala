import scala.util.continuations._

object Test {
  abstract class IfReturnRepro {
    def s1: Double @cpsParam[Any, Unit]
    def s2: Double @cpsParam[Any, Unit]

    def p(i: Int): Double @cpsParam[Unit, Any] = {
      val px = s1
      val pct = if (px > 100) px else px / s2
      println("pct = %.3f".format(pct))
      pct
    }
  }

  def main(args: Array[String]) : Unit = {
    var d: Double = 0d
    def d2 = d * d

    val irr = new IfReturnRepro {
      def s1 = shift(f => f(d))
      def s2 = shift(f => f(d2))
    }
    1 to 25 foreach { i =>
      d = i
      print("d = " + i + ", d2 = " + d2 + ", ")
      run(irr p i)
    }
  }
}
