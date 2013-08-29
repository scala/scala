// a.scala
// Thu Sep 27 09:42:16 PDT 2012

trait Bar[-T1, T2, +T3] { }
trait Foo[-T1, T2, +T3] extends Bar[T1, T2, T3]

class A {
  var b = true

  def f1(x: Foo[Int, Int, Int]) = x match {
    /* nowarn */ case _: Foo[Nothing, Int, Any] => true
  }
  def f2[T, U, V](x: Foo[T, U, V]) = x match {
    /* nowarn */ case _: Foo[Nothing, U, Any] => true
  }
  def f3[T, U, V](x: Foo[T, U, V]) = x match {
    /*   warn */ case _: Foo[U, U, V] if b       => ()
    /* nowarn */ case _: Foo[Nothing, U, V] if b => ()
    /*   warn */ case _: Foo[Any, U, V] if b     => ()
  }

  def f4(xs: List[Int]) = xs match {
    /* nowarn - todo */ case x: AnyRef { def bippy: Int } if b => x.bippy  // this could/should do an instance check and not warn
    /* nowarn - todo */ case x: AnyRef { def size: Int } if b  => x.size   // this could/should do a static conformance test and not warn
    /* nowarn */ case x: ((AnyRef { def size: Int }) @unchecked) if b  => x.size
  }
}
// unchecked-refinement.scala:14:
//     /* nowarn */ case _: Foo[Nothing, U, Any] => true
//                          ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  Foo[T,U,V]
//         pt  Foo[T,U,V]
//      pattp  Foo[Nothing,U,Any]
//   pattp+pt  Foo[Nothing,U,Any]
//   pt+pattp  Foo[T,U,V]
//     result  Foo[T,U,V]
//
//        pt0 =:= pt             pt0 ~:= pattp          pt0 ~:= pattp+pt       pt0 =:= pt+pattp       pt0 =:= result
//         pt ~:= pattp           pt ~:= pattp+pt        pt =:= pt+pattp        pt =:= result
//      pattp =:= pattp+pt     pattp ~:= pt+pattp     pattp ~:= result
//   pattp+pt ~:= pt+pattp  pattp+pt ~:= result
//   pt+pattp =:= result
//
// }// unchecked-refinement.scala:17:
//     /*   warn */ case _: Foo[U, U, V] if b       => ()
//                          ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  Foo[T,U,V]
//         pt  Foo[T,U,V]
//      pattp  Foo[U,U,V]
//   pattp+pt  Foo[U,U,V]
//   pt+pattp  Foo[T,U,V]
//     result  Foo[T,U,V]
//
//        pt0 =:= pt             pt0 ~:= pattp          pt0 ~:= pattp+pt       pt0 =:= pt+pattp       pt0 =:= result
//         pt ~:= pattp           pt ~:= pattp+pt        pt =:= pt+pattp        pt =:= result
//      pattp =:= pattp+pt     pattp ~:= pt+pattp     pattp ~:= result
//   pattp+pt ~:= pt+pattp  pattp+pt ~:= result
//   pt+pattp =:= result
//
// }// unchecked-refinement.scala:18:
//     /* nowarn */ case _: Foo[Nothing, U, V] if b => ()
//                          ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  Foo[T,U,V]
//         pt  Foo[T,U,V]
//      pattp  Foo[Nothing,U,V]
//   pattp+pt  Foo[Nothing,U,V]
//   pt+pattp  Foo[T,U,V]
//     result  Foo[T,U,V]
//
//        pt0 =:= pt             pt0 ~:= pattp          pt0 ~:= pattp+pt       pt0 =:= pt+pattp       pt0 =:= result
//         pt ~:= pattp           pt ~:= pattp+pt        pt =:= pt+pattp        pt =:= result
//      pattp =:= pattp+pt     pattp ~:= pt+pattp     pattp ~:= result
//   pattp+pt ~:= pt+pattp  pattp+pt ~:= result
//   pt+pattp =:= result
//
// }// unchecked-refinement.scala:23:
//     /* nowarn - todo */ case x: AnyRef { def bippy: Int } if b => x.bippy  // this could/should do an instance check and not warn
//                                 ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  List[Int]
//         pt  List[Int]
//      pattp  AnyRef{def bippy: Int}
//   pattp+pt  AnyRef{def bippy: Int} with List[Int]
//   pt+pattp  List[Int] with {def bippy: Int}
//     result  List[Int] with {def bippy: Int}
//
//        pt0 =:= pt             pt0 !:= pattp     pattp+pt <:< pt0       pt+pattp <:< pt0         result <:< pt0
//         pt !:= pattp     pattp+pt <:< pt        pt+pattp <:< pt          result <:< pt
//   pattp+pt <:< pattp     pt+pattp <:< pattp       result <:< pattp
//   pattp+pt ~:= pt+pattp  pattp+pt ~:= result
//   pt+pattp =:= result
//
// }