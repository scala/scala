sealed trait A2[T1]
final class B2[T1, T2] extends A2[T1]

sealed trait A[T]
final class B[T] extends A[T]

sealed trait A1[T]
trait B1[T] extends A1[T]
trait C1[T] extends A1[T]
trait D1[T] extends A1[Int]
trait E1[T] extends B1[Int]
trait F1[T] extends B1[T]

object MiscUnchecked {
  /* nowarn */ def knownType1(x: A[Int])  = x match { case _: B[Int] if true => 1 }
  /* nowarn */ def knownType2(x: B[Int])  = x match { case _: A[Int] if true => 1 }
  /* nowarn */ def tparamLeakage1(x: Any) = x match { case Array() => 1 }
  /* nowarn */ def tparamLeakage2(x: Any) = x match { case List() => 1 }

  // E1[Double] implies B1[Int], but B1[Int] does not imply E1[Double], even if .isInstanceOf[E1[_]]
  // F1[Int] implies B1[Int], and B1[Int] implies F1[Int]

  /* nowarn */ def peerTypes1(x: B1[Int]) = x match { case _: C1[Int] => true }
  /*   warn */ def peerTypes2(x: B1[Int]) = x match { case _: E1[Double] => true }
  /*   warn */ def peerTypes3(x: B1[_]) = x match { case _: F1[Double] => true }
  /* nowarn */ def peerTypes4(x: B1[Int]) = x match { case _: F1[Int] => true }

  /*   warn */ def twotypes1[T](x: B2[T, Int]) = x match { case _: A2[Int] => true }
  /* nowarn */ def twotypes2[T](x: B2[Int, T]) = x match { case _: A2[Int] => true }
  /* nowarn */ def twotypes3(x: A2[Int]) = x match { case _: B2[Int, _] => true }
  /* nowarn */ def twotypes4[T](x: A2[T]) = x match { case _: B2[T, _] => true }
  /*   warn */ def twotypes5[T](x: A2[T]) = x match { case _: B2[_, Int] => true }
}

object Arrays {
  def f1(x: Any) = x match {
    /* nowarn */ case _: Array[Int]                  => ()
    /* nowarn */ case _: Array[Boolean]              => ()
    /* nowarn */ case _: Array[String]               => ()
    /*   warn */ case _: Array[List[String]]         => ()
    /* nowarn */ case _: Array[Array[String]]        => ()
    /* nowarn */ case _: Array[Array[Array[String]]] => ()
    /*   warn */ case _: Array[Array[List[String]]]  => ()
  }

  def f2(x: Array[_]) = x match {
    /* nowarn */ case _: Array[Int]                  => ()
    /* nowarn */ case _: Array[Boolean]              => ()
    /* nowarn */ case _: Array[String]               => ()
    /*   warn */ case _: Array[List[String]]         => ()
    /* nowarn */ case _: Array[Array[String]]        => ()
    /* nowarn */ case _: Array[Array[Array[String]]] => ()
    /*   warn */ case _: Array[Array[List[String]]]  => ()
  }

  def f3[T](x: Array[T]) = x match {
    /* nowarn */ case _: Array[Int]                 => ()
    /* nowarn */ case _: Array[Boolean]             => ()
    /* nowarn */ case _: Array[String]              => ()
    /*   warn */ case _: Array[List[String]]        => ()
    /* nowarn */ case _: Array[Array[String]]       => ()
    /*   warn */ case _: Array[List[Array[String]]] => ()
    /*   warn */ case _: Array[Array[List[String]]] => ()
  }
}

object Matching {
  class Q {
    type A
    type B <: A

    def f(xs: Traversable[B]) = xs match {
      /* nowarn */ case xs: List[A] => xs.head
      /* nowarn */ case xs: Seq[B]  => xs.head
      /*   warn */ case xs: Set[A]  => xs.head
    }
    def f2[T <: B](xs: Traversable[T]) = xs match {
      /* nowarn */ case xs: List[B with T] => xs.head
      /* nowarn */ case xs: Seq[A]         => xs.head
      /* nowarn */ case xs: Set[T]         => xs.head
    }
  }
}
// unchecked3.scala:23:
//   /* nowarn */ def peerTypes1(x: B1[Int]) = x match { case _: C1[Int] => true }
//                                                               ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  B1[Int]
//         pt  B1[Int]
//      pattp  C1[Int]
//   pattp+pt  C1[Int] with B1[Int]
//   pt+pattp  B1[Int] with C1[Int]
//     result  B1[Int] with C1[Int]
//
//        pt0 =:= pt             pt0 !:= pattp     pattp+pt <:< pt0       pt+pattp <:< pt0         result <:< pt0
//         pt !:= pattp     pattp+pt <:< pt        pt+pattp <:< pt          result <:< pt
//   pattp+pt <:< pattp     pt+pattp <:< pattp       result <:< pattp
//   pattp+pt ~:= pt+pattp  pattp+pt ~:= result
//   pt+pattp =:= result
//
// }// unchecked3.scala:47:
//     /* nowarn */ case _: Array[Int]                  => ()
//                          ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  Array[_$2]
//         pt  Array[_$2]
//      pattp  Array[Int]
//   pattp+pt  Array[Int]
//   pt+pattp  Array[_$2]
//     result  Array[Int]
//
//        pt0 =:= pt             pt0 ~:= pattp          pt0 ~:= pattp+pt       pt0 =:= pt+pattp       pt0 ~:= result
//         pt ~:= pattp           pt ~:= pattp+pt        pt =:= pt+pattp        pt ~:= result
//      pattp =:= pattp+pt     pattp ~:= pt+pattp     pattp =:= result
//   pattp+pt ~:= pt+pattp  pattp+pt =:= result
//   pt+pattp ~:= result
//
// }// unchecked3.scala:48:
//     /* nowarn */ case _: Array[Boolean]              => ()
//                          ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  Array[_$2]
//         pt  Array[_$2]
//      pattp  Array[Boolean]
//   pattp+pt  Array[Boolean]
//   pt+pattp  Array[_$2]
//     result  Array[Boolean]
//
//        pt0 =:= pt             pt0 ~:= pattp          pt0 ~:= pattp+pt       pt0 =:= pt+pattp       pt0 ~:= result
//         pt ~:= pattp           pt ~:= pattp+pt        pt =:= pt+pattp        pt ~:= result
//      pattp =:= pattp+pt     pattp ~:= pt+pattp     pattp =:= result
//   pattp+pt ~:= pt+pattp  pattp+pt =:= result
//   pt+pattp ~:= result
//
// }// unchecked3.scala:49:
//     /* nowarn */ case _: Array[String]               => ()
//                          ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  Array[_$2]
//         pt  Array[_$2]
//      pattp  Array[String]
//   pattp+pt  Array[String]
//   pt+pattp  Array[_$2]
//     result  Array[String]
//
//        pt0 =:= pt             pt0 ~:= pattp          pt0 ~:= pattp+pt       pt0 =:= pt+pattp       pt0 ~:= result
//         pt ~:= pattp           pt ~:= pattp+pt        pt =:= pt+pattp        pt ~:= result
//      pattp =:= pattp+pt     pattp ~:= pt+pattp     pattp =:= result
//   pattp+pt ~:= pt+pattp  pattp+pt =:= result
//   pt+pattp ~:= result
//
// }// unchecked3.scala:50:
//     /*   warn */ case _: Array[List[String]]         => ()
//                          ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  Array[_$2]
//         pt  Array[_$2]
//      pattp  Array[List[String]]
//   pattp+pt  Array[List[String]]
//   pt+pattp  Array[_$2]
//     result  Array[List[String]]
//
//        pt0 =:= pt             pt0 ~:= pattp          pt0 ~:= pattp+pt       pt0 =:= pt+pattp       pt0 ~:= result
//         pt ~:= pattp           pt ~:= pattp+pt        pt =:= pt+pattp        pt ~:= result
//      pattp =:= pattp+pt     pattp ~:= pt+pattp     pattp =:= result
//   pattp+pt ~:= pt+pattp  pattp+pt =:= result
//   pt+pattp ~:= result
//
// }// unchecked3.scala:51:
//     /* nowarn */ case _: Array[Array[String]]        => ()
//                          ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  Array[_$2]
//         pt  Array[_$2]
//      pattp  Array[Array[String]]
//   pattp+pt  Array[Array[String]]
//   pt+pattp  Array[_$2]
//     result  Array[Array[String]]
//
//        pt0 =:= pt             pt0 ~:= pattp          pt0 ~:= pattp+pt       pt0 =:= pt+pattp       pt0 ~:= result
//         pt ~:= pattp           pt ~:= pattp+pt        pt =:= pt+pattp        pt ~:= result
//      pattp =:= pattp+pt     pattp ~:= pt+pattp     pattp =:= result
//   pattp+pt ~:= pt+pattp  pattp+pt =:= result
//   pt+pattp ~:= result
//
// }// unchecked3.scala:52:
//     /* nowarn */ case _: Array[Array[Array[String]]] => ()
//                          ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  Array[_$2]
//         pt  Array[_$2]
//      pattp  Array[Array[Array[String]]]
//   pattp+pt  Array[Array[Array[String]]]
//   pt+pattp  Array[_$2]
//     result  Array[Array[Array[String]]]
//
//        pt0 =:= pt             pt0 ~:= pattp          pt0 ~:= pattp+pt       pt0 =:= pt+pattp       pt0 ~:= result
//         pt ~:= pattp           pt ~:= pattp+pt        pt =:= pt+pattp        pt ~:= result
//      pattp =:= pattp+pt     pattp ~:= pt+pattp     pattp =:= result
//   pattp+pt ~:= pt+pattp  pattp+pt =:= result
//   pt+pattp ~:= result
//
// }// unchecked3.scala:53:
//     /*   warn */ case _: Array[Array[List[String]]]  => ()
//                          ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  Array[_$2]
//         pt  Array[_$2]
//      pattp  Array[Array[List[String]]]
//   pattp+pt  Array[Array[List[String]]]
//   pt+pattp  Array[_$2]
//     result  Array[Array[List[String]]]
//
//        pt0 =:= pt             pt0 ~:= pattp          pt0 ~:= pattp+pt       pt0 =:= pt+pattp       pt0 ~:= result
//         pt ~:= pattp           pt ~:= pattp+pt        pt =:= pt+pattp        pt ~:= result
//      pattp =:= pattp+pt     pattp ~:= pt+pattp     pattp =:= result
//   pattp+pt ~:= pt+pattp  pattp+pt =:= result
//   pt+pattp ~:= result
//
// }// unchecked3.scala:57:
//     /* nowarn */ case _: Array[Int]                 => ()
//                          ^
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
// }// unchecked3.scala:58:
//     /* nowarn */ case _: Array[Boolean]             => ()
//                          ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  Array[T]
//         pt  Array[T]
//      pattp  Array[Boolean]
//   pattp+pt  Array[Boolean]
//   pt+pattp  Array[T]
//     result  Array[Boolean]
//
//        pt0 =:= pt             pt0 ~:= pattp          pt0 ~:= pattp+pt       pt0 =:= pt+pattp       pt0 ~:= result
//         pt ~:= pattp           pt ~:= pattp+pt        pt =:= pt+pattp        pt ~:= result
//      pattp =:= pattp+pt     pattp ~:= pt+pattp     pattp =:= result
//   pattp+pt ~:= pt+pattp  pattp+pt =:= result
//   pt+pattp ~:= result
//
// }// unchecked3.scala:59:
//     /* nowarn */ case _: Array[String]              => ()
//                          ^
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
// }// unchecked3.scala:60:
//     /*   warn */ case _: Array[List[String]]        => ()
//                          ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  Array[T]
//         pt  Array[T]
//      pattp  Array[List[String]]
//   pattp+pt  Array[List[String]]
//   pt+pattp  Array[T]
//     result  Array[List[String]]
//
//        pt0 =:= pt             pt0 ~:= pattp          pt0 ~:= pattp+pt       pt0 =:= pt+pattp       pt0 ~:= result
//         pt ~:= pattp           pt ~:= pattp+pt        pt =:= pt+pattp        pt ~:= result
//      pattp =:= pattp+pt     pattp ~:= pt+pattp     pattp =:= result
//   pattp+pt ~:= pt+pattp  pattp+pt =:= result
//   pt+pattp ~:= result
//
// }// unchecked3.scala:61:
//     /* nowarn */ case _: Array[Array[String]]       => ()
//                          ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  Array[T]
//         pt  Array[T]
//      pattp  Array[Array[String]]
//   pattp+pt  Array[Array[String]]
//   pt+pattp  Array[T]
//     result  Array[Array[String]]
//
//        pt0 =:= pt             pt0 ~:= pattp          pt0 ~:= pattp+pt       pt0 =:= pt+pattp       pt0 ~:= result
//         pt ~:= pattp           pt ~:= pattp+pt        pt =:= pt+pattp        pt ~:= result
//      pattp =:= pattp+pt     pattp ~:= pt+pattp     pattp =:= result
//   pattp+pt ~:= pt+pattp  pattp+pt =:= result
//   pt+pattp ~:= result
//
// }// unchecked3.scala:62:
//     /*   warn */ case _: Array[List[Array[String]]] => ()
//                          ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  Array[T]
//         pt  Array[T]
//      pattp  Array[List[Array[String]]]
//   pattp+pt  Array[List[Array[String]]]
//   pt+pattp  Array[T]
//     result  Array[List[Array[String]]]
//
//        pt0 =:= pt             pt0 ~:= pattp          pt0 ~:= pattp+pt       pt0 =:= pt+pattp       pt0 ~:= result
//         pt ~:= pattp           pt ~:= pattp+pt        pt =:= pt+pattp        pt ~:= result
//      pattp =:= pattp+pt     pattp ~:= pt+pattp     pattp =:= result
//   pattp+pt ~:= pt+pattp  pattp+pt =:= result
//   pt+pattp ~:= result
//
// }// unchecked3.scala:63:
//     /*   warn */ case _: Array[Array[List[String]]] => ()
//                          ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  Array[T]
//         pt  Array[T]
//      pattp  Array[Array[List[String]]]
//   pattp+pt  Array[Array[List[String]]]
//   pt+pattp  Array[T]
//     result  Array[Array[List[String]]]
//
//        pt0 =:= pt             pt0 ~:= pattp          pt0 ~:= pattp+pt       pt0 =:= pt+pattp       pt0 ~:= result
//         pt ~:= pattp           pt ~:= pattp+pt        pt =:= pt+pattp        pt ~:= result
//      pattp =:= pattp+pt     pattp ~:= pt+pattp     pattp =:= result
//   pattp+pt ~:= pt+pattp  pattp+pt =:= result
//   pt+pattp ~:= result
//
// }// unchecked3.scala:75:
//       /*   warn */ case xs: Set[A]  => xs.head
//                             ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  Traversable[Q.this.B]
//         pt  Traversable[Q.this.B]
//      pattp  Set[Q.this.A]
//   pattp+pt  Set[Q.this.A] with scala.collection.immutable.Set[Q.this.B]
//   pt+pattp  scala.collection.immutable.Set[Q.this.B] with Set[Q.this.A]
//     result  scala.collection.immutable.Set[Q.this.B] with Set[Q.this.A]
//
//        pt0 =:= pt             pt0 !:= pattp     pattp+pt <:< pt0       pt+pattp <:< pt0         result <:< pt0
//         pt !:= pattp     pattp+pt <:< pt        pt+pattp <:< pt          result <:< pt
//   pattp+pt <:< pattp     pt+pattp <:< pattp       result <:< pattp
//   pattp+pt ~:= pt+pattp  pattp+pt ~:= result
//   pt+pattp =:= result
//
// }