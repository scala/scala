package a {
  abstract class DeliteOp[B]
  abstract class DeliteCollection[A]
  abstract class Exp[T] { def Type: T }

  trait DeliteOpMap[A,B,C[X] <: DeliteCollection[X]] extends DeliteOp[C[B]] {
    val in: Exp[C[A]]
    val func: Exp[B]
    val alloc: Exp[C[B]]
  }

  object Test {
    def f(x: DeliteOp[_]) = x match {
      case map: DeliteOpMap[_,_,_] => map.alloc.Type
    }
  }
}

package b {
  object Test {
    def f(x: DeliteOp[_]) = x match {
      case map: DeliteOpMap[_,_,_] => map.alloc.Type
    }
  }

  abstract class DeliteOp[B]
  abstract class DeliteCollection[A]
  abstract class Exp[T] { def Type: T }

  trait DeliteOpMap[A,B,C[X] <: DeliteCollection[X]] extends DeliteOp[C[B]] {
    val in: Exp[C[A]]
    val func: Exp[B]
    val alloc: Exp[C[B]]
  }
}