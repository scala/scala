import language.existentials

class Result[+A]

case class Success[A](x: A) extends Result[A]

class Apply[A]

object Apply {
  def apply[A](f: Int => Result[A]): Apply[A] = new Apply[A]
}

object TestUnit {
  //Error is here:
  def goo = Apply { i =>
    i match {
      case 1 => Success(Some(1))
      case _ => Success(None)
    }
  }
  
  //If type is defined explicitly (which I wanted from compiler to infer), then all is ok
  def foo = Apply[t forSome { type t >: Some[Int] with None.type <: Option[Int] }] { i =>
    i match {
      case 1 => Success(Some(1))
      case _ => Success(None)
    }
  }
}
