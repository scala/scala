object LeftOrRight {
  def unapply[A](value: Either[A, A]): Option[A] = value match {
    case scala.Left(x)  => Some(x)
    case scala.Right(x) => Some(x)
  }
}

object NativelyTwo {
  def unapply[A](value: Either[A, A]): Option[(A, A)] = value match {
    case scala.Left(x)  => Some(x -> x)
    case scala.Right(x) => Some(x -> x)
  }
}


class E {
  def f1 = (Left((0, 0)): Either[(Int, Int), (Int, Int)]) match { case LeftOrRight(a) => a  }          // warn
  def f2 = (Left((0, 0)): Either[(Int, Int), (Int, Int)]) match { case LeftOrRight((a, b)) => a  }     // no warn
  def f3 = (Left((0, 0)): Either[(Int, Int), (Int, Int)]) match { case LeftOrRight((a, b, c)) => a  }  // fail
}

class B {
  def f1[A](x: A) = (Left(x): Either[A, A])                match { case LeftOrRight(a) => a  }          // no warn
  def f2[A](x: A) = (Left(x -> x): Either[(A, A), (A, A)]) match { case LeftOrRight(a) => a  }          // warn
  def f3[A](x: A) = (Left(x -> x): Either[(A, A), (A, A)]) match { case LeftOrRight((a, b)) => a  }     // no warn
  def f4[A](x: A) = (Left(x -> x): Either[(A, A), (A, A)]) match { case LeftOrRight((a, b, c)) => a  }  // fail
}

class C {
  def f1 = (Left((0, 0)): Either[(Int, Int), (Int, Int)]) match { case NativelyTwo(a) => a  }          // warn
  def f2 = (Left((0, 0)): Either[(Int, Int), (Int, Int)]) match { case NativelyTwo((a, b)) => a  }     // no warn
  def f3 = (Left((0, 0)): Either[(Int, Int), (Int, Int)]) match { case NativelyTwo((a, b, c)) => a  }  // fail
}

class D {
  def f1[A](x: A) = (Left(x): Either[A, A])                match { case NativelyTwo(a) => a  }          // warn
  def f2[A](x: A) = (Left(x -> x): Either[(A, A), (A, A)]) match { case NativelyTwo(a) => a  }          // warn
  def f3[A](x: A) = (Left(x -> x): Either[(A, A), (A, A)]) match { case NativelyTwo((a, b)) => a  }     // no warn
  def f4[A](x: A) = (Left(x -> x): Either[(A, A), (A, A)]) match { case NativelyTwo((a, b, c)) => a  }  // fail
}
