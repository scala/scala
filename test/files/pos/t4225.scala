
//> using options -Yrangepos
//
object Test {
  class Foo {
    class Bar
    object Bar {
      implicit def mkBar: Bar = new Bar
    }
  }

  object f extends Foo

  object ImplicitClass {
    implicit class Ops[F <: Foo](val f0: F) {
      def op0(implicit b: f0.Bar): f0.Bar = b
      def op1(i: Int)(implicit b: f0.Bar): f0.Bar = b
      def op2(i: Int = 23)(implicit b: f0.Bar): f0.Bar = b
      def op3(i: Int)(j: Boolean)(implicit b: f0.Bar): f0.Bar = b
      def op4[T](implicit b: f0.Bar): f0.Bar = b
      def op5(i: => Int)(implicit b: f0.Bar): f0.Bar = b
    }

    f.op0
    f.op1(23)
    f.op2()
    f.op3(23)(true)
    f.op4[Int]
    f.op5(23)
  }

  object LazyImplicitConversion {
    class Ops[F <: Foo](val f0: F) {
      def op0(implicit b: f0.Bar): f0.Bar = b
      def op1(i: Int)(implicit b: f0.Bar): f0.Bar = b
      def op2(i: Int = 23)(implicit b: f0.Bar): f0.Bar = b
      def op3(i: Int)(j: Boolean)(implicit b: f0.Bar): f0.Bar = b
      def op4[T](implicit b: f0.Bar): f0.Bar = b
      def op5(i: => Int)(implicit b: f0.Bar): f0.Bar = b
    }
    implicit def ops[F <: Foo](f: => F): Ops[F] = new Ops(f)

    f.op0
    f.op1(23)
    f.op2()
    f.op3(23)(true)
    f.op4[Int]
    f.op5(23)
  }

  object RightAssociativeOps {
    implicit class Ops[F <: Foo](val f0: F) {
      def op0_:(i: Int)(implicit b: f0.Bar): f0.Bar = b
      def op1_:(i: => Int)(implicit b: f0.Bar): f0.Bar = b
    }

    object f extends Foo

    23 op0_: f
    23 op1_: f
  }

  object Blocks {
    implicit class Ops[F <: Foo](val f0: F) {
      def op0(implicit b: f0.Bar): f0.Bar = b
    }

    { class Foo ; f }.op0

    { object f1 extends Foo ; f1 }.op0

    // The above expands to the following ...
    val stab0 =
      new Ops({
        object f1 extends Foo
        f1
      })
    stab0.op0(stab0.f0.Bar.mkBar)

    { val f1 = new Foo ; f1 }.op0

    // The above expands to the following ...
    val stab1 =
      new Ops({
        val f1: Foo = new Foo
        f1
      })
    stab1.op0(stab1.f0.Bar.mkBar)
  }
}
