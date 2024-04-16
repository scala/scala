//> using options -Yrangepos
//
import scala.language.implicitConversions

object Test extends App {
  class Foo {
    class Bar
    object Bar {
      implicit def mkBar: Bar = {
        println("mkBar")
        new Bar
      }
    }
  }

  def effect: Int = { println("effect") ; 13 }

  {
    implicit class Ops[F <: Foo](val f0: F) {
      println("Ops")
      def op0(i: Int)(implicit b: f0.Bar): f0.Bar = { println(i) ; b }
      def op1(i: => Int)(implicit b: f0.Bar): f0.Bar = { println(i) ; b }
    }

    object f0 extends Foo {
      println("f0")
    }

    f0.op0(effect)
    println()

    object f1 extends Foo {
      println("f1")
    }

    f1.op1(effect)
    println()
  }

  {
    class Ops[F <: Foo](val f0: F) {
      def op0(i: Int)(implicit b: f0.Bar): f0.Bar = { println(i) ; b }
      def op1(i: => Int)(implicit b: f0.Bar): f0.Bar = { println(i) ; b }
    }
    implicit def ops[F <: Foo](f: => F): Ops[F] = {
      println("Ops")
      new Ops(f)
    }

    object f0 extends Foo {
      println("f0")
    }

    f0.op0(effect)
    println()

    object f1 extends Foo {
      println("f1")
    }

    f1.op1(effect)
    println()
  }
}
