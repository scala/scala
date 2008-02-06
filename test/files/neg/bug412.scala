object Magic {

  abstract class A[T1,T2]() {
    trait C            { type T; }
    trait C1 extends C { type T = T1; }
    trait C2 extends C { type T <: T2; }

    type CX >: Null;
    val c: CX with C2 = null;

    def castA(x: c.T): T2 = x;
  }

  class B[T1,T2] extends A[T1,T2]() {
    type CX = C1;

    def castB(x: T1): T2 = castA(x);
  }

  def cast[T1,T2](v: T1): T2 =
    new B[T1,T2]().castB(v)

}

object Test {

  def main(args: Array[String]): Unit = {
    Magic.cast[String,Exception]("xyz").printStackTrace();
  }

}
