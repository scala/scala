object Test {
  trait MinimalInf {
    def infix_/(lhs: Rep[Int], rhs: Rep[Int]): Rep[Int]
    class Rep[T]
    abstract class Ops[T](x: Rep[T]) {
      def *(y: Rep[T]): Rep[T]
      def /(y: Rep[T]): Rep[T]
    }
    implicit def doubleToOps(x:Double): Ops[Double]
    implicit def doubleRepToOps(x:Rep[Double]): Ops[Double]
    def sqrt(x: Rep[Double]): Rep[Double]
    def acos(x: Rep[Double]): Rep[Double]
    def unit(x:Double): Rep[Double]
  }

  trait Prog extends MinimalInf {
    def test1 = {
      val r = unit(6.0)
      val q = unit(6.0)
      val theta = r/sqrt(-1.0*q*q*q*q)
    }
    def test2 = {
      val numRows = unit(7)
      val xstep = 25.0/numRows 
    }
  }
}
