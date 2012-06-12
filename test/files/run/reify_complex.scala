import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

object Test extends App {
  reify {
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
  }.eval
}