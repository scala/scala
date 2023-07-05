sealed trait A
sealed trait B extends A

trait F[C] {
  type T = C
}

object O extends F[B]

// 2.13.10: passes
// 2.13.11: cyclic in patmat
// 2.13.12: passes
object P1 extends F[O.T] {
  val f: PartialFunction[A, P1.T] = {
    case x: P1.T => x
  }
}

// 2.13.10: cyclic in patmat
// 2.13.11: cyclic in patmat
// 2.13.12: passes
object P2 extends F[O.T] {
  val f: PartialFunction[A, P2.T] = x => x match {
    case x: P2.T => x
  }
}

// 2.13.10: cyclic in uncurry
// 2.13.11: cyclic in patmat
// 2.13.12: cyclic in uncurry
object P3 extends F[O.T] {
  val f: Function1[A, P3.T] = {
    case x: P3.T => x
  }
}

// 2.13.10: cyclic in uncurry
// 2.13.11: cyclic in patmat
// 2.13.12: cyclic in uncurry
object P4 extends F[O.T] {
  val f: Function1[A, P4.T] = x => x match {
    case x: P4.T => x
  }
}
