abstract class FooA {
    type A <: AxA;
    abstract class AxA;
    abstract class InnerA {
      type B <: A;
      def doB : B;
    }
  }
  trait FooB extends FooA {
    type A <: AxB;
    trait AxB extends AxA { def xxx : Int; }
    abstract class InnerB extends InnerA {
      // type B <: A;
      val a : A = doB;
      a.xxx;
      val aaa: InnerB.this.B = doB
      aaa.xxx;
    }
  } 
