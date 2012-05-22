sealed trait Option {}
case class Choice(a: Option, b: Option) extends Option;
case class Some(x: Boolean) extends Option;
case object None extends Option;

object test {

// drop any case and it will report an error
// note that booleans are taken into account
  def f(opt: Option) = (opt: @unchecked) match {
    case Choice(None, None) => 1;
    case Choice(None, Some(_)) => 1;
    case Choice(None, Choice(_, _)) => 1;
    case Choice(Some(true), None) => 1;
    // case Choice(Some(false), None) => 1;
    case Choice(Some(_), Some(_)) => 1;
    case Choice(Some(_), Choice(_, _)) => 1;
    case Choice(Choice(_, _), None) => 1;
    case Choice(Choice(_, _), Some(_)) => 1;
    case Choice(Choice(_, _), Choice(_, _)) => 1;
    case Some(b) => 4;
    case None => 5;
  }
}
