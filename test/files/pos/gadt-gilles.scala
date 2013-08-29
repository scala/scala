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
// gadt-gilles.scala:33:
//    case t: T[u, v] => new T[u, v]{}
//            ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  Test2.T[Int,Int]
//         pt  Test2.T[Int,Int]
//      pattp  Test2.T[u,v]
//   pattp+pt  Test2.T[u,v]
//   pt+pattp  Test2.T[Int,Int]
//     result  Test2.T[Int,Int]
//
//        pt0 =:= pt             pt0 ~:= pattp          pt0 ~:= pattp+pt       pt0 =:= pt+pattp       pt0 =:= result
//         pt ~:= pattp           pt ~:= pattp+pt        pt =:= pt+pattp        pt =:= result
//      pattp =:= pattp+pt     pattp ~:= pt+pattp     pattp ~:= result
//   pattp+pt ~:= pt+pattp  pattp+pt ~:= result
//   pt+pattp =:= result
//
// }