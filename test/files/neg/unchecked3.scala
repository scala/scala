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
