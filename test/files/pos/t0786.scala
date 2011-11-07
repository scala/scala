object ImplicitProblem {
  class M[T]

  def nullval[T] = null.asInstanceOf[T];
    
  trait Rep[T] {
    def eval: Int
  }
    
  implicit def toRep0(n: Int) = new Rep[Int] {
    def eval = 0
  }
    
  implicit def toRepN[T](n: M[T])(implicit f: T => Rep[T]) = new Rep[M[T]] {
    def eval = f(nullval[T]).eval + 1
  }

  def depth[T <% Rep[T]](n: T) = n.eval

  def main(args: Array[String]) {
    println(depth(nullval[M[Int]]))  // (1) this works   
    println(nullval[M[Int]].eval)    // (2) this works
      
    type m = M[Int]
    println(depth(nullval[m]))     // (3) this doesn't compile on 2.7.RC1    
    println(nullval[m].eval)       // (4) this works
  }

}
