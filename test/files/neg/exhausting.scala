object Test {
  sealed abstract class Foo[T]
  case object Bar1 extends Foo[Int]
  case object Bar2 extends Foo[String]
  case object Bar3 extends Foo[Any]
  
  def ex1[T](xs: List[T]) = xs match {
    case ys: List[_]  => "ok"
  }
  def ex2[T](xx: (Foo[T], Foo[T])) = xx match {
    case (Bar1, Bar1) => ()
    case (_, Bar1) => ()
    case (_, Bar3) => ()
    case (_, Bar2) => ()
  }
  def ex3[T](xx: (Foo[T], Foo[T])) = xx match {
    case (_: Foo[_], _: Foo[_]) => ()
  }

  // fails for: ::(_, Nil), ::(_, ::(_, ::(_, _))), ...
  def fail1[T](xs: List[T]) = xs match {
    case Nil            => "ok"
    case x :: y :: Nil  => "ok"
  }

  // fails for: Nil
  def fail2[T](xs: List[T]) = xs match {
    case _ :: _ => "ok"
  }

  // fails for: ::(<not in (2, 1)>, _)
  def fail3a(xs: List[Int]) = xs match {
    case 1 :: _ =>
    case 2 :: _ =>
    case Nil =>
  }

  // fails for: Bar3
  def fail3[T](x: Foo[T]) = x match {
    case Bar1   => "ok"
    case Bar2   => "ok"
  }
  // fails for: (Bar1, Bar2)
  // fails for: (Bar1, Bar3)
  // fails for: (Bar2, Bar2)
  // fails for: (Bar2, Bar1)
  def fail4[T <: AnyRef](xx: (Foo[T], Foo[T])) = xx match {
    case (Bar1, Bar1) => ()
    case (Bar2, Bar3) => ()
    case (Bar3, _) => ()
  }
  // fails for: (Bar1, Bar2)
  // fails for: (Bar1, Bar3)
  // fails for: (Bar2, Bar1)
  // fails for: (Bar2, Bar2)
  def fail5[T](xx: (Foo[T], Foo[T])) = xx match {
    case (Bar1, Bar1) => ()
    case (Bar2, Bar3) => ()
    case (Bar3, _) => ()
  }
}
// exhausting.scala:17:
//     case (_: Foo[_], _: Foo[_]) => ()
//              ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  Test.Foo[T]
//         pt  Test.Foo[T]
//      pattp  Test.Foo[_]
//   pattp+pt  Test.Foo[_]
//   pt+pattp  Test.Foo[T]
//     result  Test.Foo[T]
//
//        pt0 =:= pt             pt0 ~:= pattp          pt0 ~:= pattp+pt       pt0 =:= pt+pattp       pt0 =:= result
//         pt ~:= pattp           pt ~:= pattp+pt        pt =:= pt+pattp        pt =:= result
//      pattp =:= pattp+pt     pattp ~:= pt+pattp     pattp ~:= result
//   pattp+pt ~:= pt+pattp  pattp+pt ~:= result
//   pt+pattp =:= result
//
// }// exhausting.scala:17:
//     case (_: Foo[_], _: Foo[_]) => ()
//                         ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  Test.Foo[T]
//         pt  Test.Foo[T]
//      pattp  Test.Foo[_]
//   pattp+pt  Test.Foo[_]
//   pt+pattp  Test.Foo[T]
//     result  Test.Foo[T]
//
//        pt0 =:= pt             pt0 ~:= pattp          pt0 ~:= pattp+pt       pt0 =:= pt+pattp       pt0 =:= result
//         pt ~:= pattp           pt ~:= pattp+pt        pt =:= pt+pattp        pt =:= result
//      pattp =:= pattp+pt     pattp ~:= pt+pattp     pattp ~:= result
//   pattp+pt ~:= pt+pattp  pattp+pt ~:= result
//   pt+pattp =:= result
//
// }