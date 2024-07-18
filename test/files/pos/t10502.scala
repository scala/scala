//> using options -Xfatal-warnings

final class Box[A](val unwrap: A)

object Box {
  def apply[A](x: A): Box[A]         = new Box[A](x)
  def unapply[A](x: Box[A]): Some[A] = Some(x.unwrap)
}

object Perhaps {
  def unapply[A](oa: Option[A]): Some[Option[A]] = Some(oa)
}

class Test {
  def test() = {
    List(Option("hello")) match {
      case Perhaps(Some(s)) :: _ =>
      case Perhaps(None   ) :: _ =>
      case Nil                   =>
    }
  }

  def justOption() = {
    Option("hello") match {
      case Perhaps(Some(s)) =>
      case Perhaps(None)    =>
    }
  }

  def boxTest() = {
    Box(Option("hello")) match {
      case Box(Some(s)) =>
      case Box(None)    =>
    }
  }
}
