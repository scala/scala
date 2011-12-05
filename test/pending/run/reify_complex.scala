import scala.tools.nsc.reporters._
import scala.tools.nsc.Settings
import reflect.runtime.Mirror.ToolBox

object Test extends App {
  val code = scala.reflect.Code.lift{
    class Complex(val re: Double, val im: Double) {
      def + (that: Complex) =
        new Complex(re + that.re, im + that.im)
      def - (that: Complex) =
        new Complex(re - that.re, im - that.im)
      def * (that: Complex) =
        new Complex(re * that.re - im * that.im,
                    re * that.im + im * that.re)
      def / (that: Complex) = {
        val denom = that.re * that.re + that.im * that.im
        new Complex((re * that.re + im * that.im) / denom,
                    (im * that.re - re * that.im) / denom)
      }
      override def toString =
        re + (if (im < 0) "-" + (-im) else "+" + im) + "*i"
    }
    val x = new Complex(2, 1); val y = new Complex(1, 3)
    println(x + y)
  };

  val reporter = new ConsoleReporter(new Settings)
  val toolbox = new ToolBox(reporter)
  val ttree = toolbox.typeCheck(code.tree)
  toolbox.runExpr(ttree)
}
