// Test cases: the only place we can cut and paste without crying
// ourself to sleep.
object Test {
  def f1(a: Any)  = a match {
    case x: Array[Int]      => x(0)
    case x: Array[Double]   => 2
    case x: Array[Float]    => x.sum.toInt
    case x: Array[String]   => x.size
    case x: Array[AnyRef]   => 5
    case x: Array[_]        => 6
    case _                  => 7
  }
  def f2(a: Array[_])  = a match {
    case x: Array[Int]      => x(0)
    case x: Array[Double]   => 2
    case x: Array[Float]    => x.sum.toInt
    case x: Array[String]   => x.size
    case x: Array[AnyRef]   => 5
    case x: Array[_]        => 6
    case _                  => 7
  }
  def f3[T](a: Array[T]) = a match {
    case x: Array[Int]      => x(0)
    case x: Array[Double]   => 2
    case x: Array[Float]    => x.sum.toInt
    case x: Array[String]   => x.size
    case x: Array[AnyRef]   => 5
    case x: Array[_]        => 6
    case _                  => 7
  }
  

  def main(args: Array[String]): Unit = {
    println(f1(Array(1, 2, 3)))
    println(f1(Array(1.0, -2.0, 3.0, 1.0)))
    println(f1(Array(1.0f, 2.0f, 3.0f, -3.0f)))
    println(f1((1 to 4).toArray map (_.toString)))
    println(f1(new Array[Any](10)))    // should match as Array[AnyRef]
    println(f1(Array(1L)))
    println(f1(null))
    
    println(f2(Array(1, 2, 3)))
    println(f2(Array(1.0, -2.0, 3.0, 1.0)))
    println(f2(Array(1.0f, 2.0f, 3.0f, -3.0f)))
    println(f2((1 to 4).toArray map (_.toString)))
    println(f2(new Array[Any](10)))    // should match as Array[AnyRef]
    println(f2(Array(1L)))
    println(f2(null))
    
    println(f3(Array(1, 2, 3)))
    println(f3(Array(1.0, -2.0, 3.0, 1.0)))
    println(f3(Array(1.0f, 2.0f, 3.0f, -3.0f)))
    println(f3((1 to 4).toArray map (_.toString)))
    println(f3(new Array[Any](10)))    // should match as Array[AnyRef]
    println(f3(Array(1L)))
    println(f3(null))
  }
}
// t2755.scala:14:
//     case x: Array[Int]      => x(0)
//             ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  Array[_$1]
//         pt  Array[_$1]
//      pattp  Array[Int]
//   pattp+pt  Array[Int]
//   pt+pattp  Array[_$1]
//     result  Array[Int]
//
//        pt0 =:= pt             pt0 ~:= pattp          pt0 ~:= pattp+pt       pt0 =:= pt+pattp       pt0 ~:= result
//         pt ~:= pattp           pt ~:= pattp+pt        pt =:= pt+pattp        pt ~:= result
//      pattp =:= pattp+pt     pattp ~:= pt+pattp     pattp =:= result
//   pattp+pt ~:= pt+pattp  pattp+pt =:= result
//   pt+pattp ~:= result
//
// }// t2755.scala:15:
//     case x: Array[Double]   => 2
//             ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  Array[_$1]
//         pt  Array[_$1]
//      pattp  Array[Double]
//   pattp+pt  Array[Double]
//   pt+pattp  Array[_$1]
//     result  Array[Double]
//
//        pt0 =:= pt             pt0 ~:= pattp          pt0 ~:= pattp+pt       pt0 =:= pt+pattp       pt0 ~:= result
//         pt ~:= pattp           pt ~:= pattp+pt        pt =:= pt+pattp        pt ~:= result
//      pattp =:= pattp+pt     pattp ~:= pt+pattp     pattp =:= result
//   pattp+pt ~:= pt+pattp  pattp+pt =:= result
//   pt+pattp ~:= result
//
// }// t2755.scala:16:
//     case x: Array[Float]    => x.sum.toInt
//             ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  Array[_$1]
//         pt  Array[_$1]
//      pattp  Array[Float]
//   pattp+pt  Array[Float]
//   pt+pattp  Array[_$1]
//     result  Array[Float]
//
//        pt0 =:= pt             pt0 ~:= pattp          pt0 ~:= pattp+pt       pt0 =:= pt+pattp       pt0 ~:= result
//         pt ~:= pattp           pt ~:= pattp+pt        pt =:= pt+pattp        pt ~:= result
//      pattp =:= pattp+pt     pattp ~:= pt+pattp     pattp =:= result
//   pattp+pt ~:= pt+pattp  pattp+pt =:= result
//   pt+pattp ~:= result
//
// }// t2755.scala:17:
//     case x: Array[String]   => x.size
//             ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  Array[_$1]
//         pt  Array[_$1]
//      pattp  Array[String]
//   pattp+pt  Array[String]
//   pt+pattp  Array[_$1]
//     result  Array[String]
//
//        pt0 =:= pt             pt0 ~:= pattp          pt0 ~:= pattp+pt       pt0 =:= pt+pattp       pt0 ~:= result
//         pt ~:= pattp           pt ~:= pattp+pt        pt =:= pt+pattp        pt ~:= result
//      pattp =:= pattp+pt     pattp ~:= pt+pattp     pattp =:= result
//   pattp+pt ~:= pt+pattp  pattp+pt =:= result
//   pt+pattp ~:= result
//
// }// t2755.scala:18:
//     case x: Array[AnyRef]   => 5
//             ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  Array[_$1]
//         pt  Array[_$1]
//      pattp  Array[AnyRef]
//   pattp+pt  Array[AnyRef]
//   pt+pattp  Array[_$1]
//     result  Array[AnyRef]
//
//        pt0 =:= pt             pt0 ~:= pattp          pt0 ~:= pattp+pt       pt0 =:= pt+pattp       pt0 ~:= result
//         pt ~:= pattp           pt ~:= pattp+pt        pt =:= pt+pattp        pt ~:= result
//      pattp =:= pattp+pt     pattp ~:= pt+pattp     pattp =:= result
//   pattp+pt ~:= pt+pattp  pattp+pt =:= result
//   pt+pattp ~:= result
//
// }// t2755.scala:19:
//     case x: Array[_]        => 6
//             ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  Array[_$1]
//         pt  Array[_$1]
//      pattp  Array[_]
//   pattp+pt  Array[_]
//   pt+pattp  Array[_$1]
//     result  Array[_$1]
//
//        pt0 =:= pt             pt0 ~:= pattp          pt0 ~:= pattp+pt       pt0 =:= pt+pattp       pt0 =:= result
//         pt ~:= pattp           pt ~:= pattp+pt        pt =:= pt+pattp        pt =:= result
//      pattp =:= pattp+pt     pattp ~:= pt+pattp     pattp ~:= result
//   pattp+pt ~:= pt+pattp  pattp+pt ~:= result
//   pt+pattp =:= result
//
// }// t2755.scala:23:
//     case x: Array[Int]      => x(0)
//             ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  Array[T]
//         pt  Array[T]
//      pattp  Array[Int]
//   pattp+pt  Array[Int]
//   pt+pattp  Array[T]
//     result  Array[Int]
//
//        pt0 =:= pt             pt0 ~:= pattp          pt0 ~:= pattp+pt       pt0 =:= pt+pattp       pt0 ~:= result
//         pt ~:= pattp           pt ~:= pattp+pt        pt =:= pt+pattp        pt ~:= result
//      pattp =:= pattp+pt     pattp ~:= pt+pattp     pattp =:= result
//   pattp+pt ~:= pt+pattp  pattp+pt =:= result
//   pt+pattp ~:= result
//
// }// t2755.scala:24:
//     case x: Array[Double]   => 2
//             ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  Array[T]
//         pt  Array[T]
//      pattp  Array[Double]
//   pattp+pt  Array[Double]
//   pt+pattp  Array[T]
//     result  Array[Double]
//
//        pt0 =:= pt             pt0 ~:= pattp          pt0 ~:= pattp+pt       pt0 =:= pt+pattp       pt0 ~:= result
//         pt ~:= pattp           pt ~:= pattp+pt        pt =:= pt+pattp        pt ~:= result
//      pattp =:= pattp+pt     pattp ~:= pt+pattp     pattp =:= result
//   pattp+pt ~:= pt+pattp  pattp+pt =:= result
//   pt+pattp ~:= result
//
// }// t2755.scala:25:
//     case x: Array[Float]    => x.sum.toInt
//             ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  Array[T]
//         pt  Array[T]
//      pattp  Array[Float]
//   pattp+pt  Array[Float]
//   pt+pattp  Array[T]
//     result  Array[Float]
//
//        pt0 =:= pt             pt0 ~:= pattp          pt0 ~:= pattp+pt       pt0 =:= pt+pattp       pt0 ~:= result
//         pt ~:= pattp           pt ~:= pattp+pt        pt =:= pt+pattp        pt ~:= result
//      pattp =:= pattp+pt     pattp ~:= pt+pattp     pattp =:= result
//   pattp+pt ~:= pt+pattp  pattp+pt =:= result
//   pt+pattp ~:= result
//
// }// t2755.scala:26:
//     case x: Array[String]   => x.size
//             ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  Array[T]
//         pt  Array[T]
//      pattp  Array[String]
//   pattp+pt  Array[String]
//   pt+pattp  Array[T]
//     result  Array[String]
//
//        pt0 =:= pt             pt0 ~:= pattp          pt0 ~:= pattp+pt       pt0 =:= pt+pattp       pt0 ~:= result
//         pt ~:= pattp           pt ~:= pattp+pt        pt =:= pt+pattp        pt ~:= result
//      pattp =:= pattp+pt     pattp ~:= pt+pattp     pattp =:= result
//   pattp+pt ~:= pt+pattp  pattp+pt =:= result
//   pt+pattp ~:= result
//
// }// t2755.scala:27:
//     case x: Array[AnyRef]   => 5
//             ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  Array[T]
//         pt  Array[T]
//      pattp  Array[AnyRef]
//   pattp+pt  Array[AnyRef]
//   pt+pattp  Array[T]
//     result  Array[AnyRef]
//
//        pt0 =:= pt             pt0 ~:= pattp          pt0 ~:= pattp+pt       pt0 =:= pt+pattp       pt0 ~:= result
//         pt ~:= pattp           pt ~:= pattp+pt        pt =:= pt+pattp        pt ~:= result
//      pattp =:= pattp+pt     pattp ~:= pt+pattp     pattp =:= result
//   pattp+pt ~:= pt+pattp  pattp+pt =:= result
//   pt+pattp ~:= result
//
// }// t2755.scala:28:
//     case x: Array[_]        => 6
//             ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  Array[T]
//         pt  Array[T]
//      pattp  Array[_]
//   pattp+pt  Array[_]
//   pt+pattp  Array[T]
//     result  Array[T]
//
//        pt0 =:= pt             pt0 ~:= pattp          pt0 ~:= pattp+pt       pt0 =:= pt+pattp       pt0 =:= result
//         pt ~:= pattp           pt ~:= pattp+pt        pt =:= pt+pattp        pt =:= result
//      pattp =:= pattp+pt     pattp ~:= pt+pattp     pattp ~:= result
//   pattp+pt ~:= pt+pattp  pattp+pt ~:= result
//   pt+pattp =:= result
//
// }