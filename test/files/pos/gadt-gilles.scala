object Test {
    trait A[T]
    trait B[U, V] extends A[U with V] // indirect constraint
    trait C
    trait D

  val x: A[C with D] = new B[C, D] {}
  val y: A[C with D] = x match { case b: B[u, v] => (new B[u, v] {}): A[u with v] } // OK
  

  def f[T, U](p: A[T with U]): A[T with U] = p match { case b: B[u, v] => new A[u with v] {} } // Not OK
}

object Test1 {

 trait T[U, V <: U]

 def f(r: Any) = r match {

   case t: T[u, v] => new T[u, v]{}

 }

}
object Test2 {

 trait T[U, V <: U]

 val x: T[Int, Int] = new T[Int, Int]{}

 x match {

   case t: T[u, v] => new T[u, v]{}

 }

}
