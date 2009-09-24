trait Option {}
case class Choice(a: Option, b: Option) extends Option;
case class Some(x: java.lang.String) extends Option;
case object None extends Option;

object test {

  def f(opt: Option) = opt match {
    case Choice(Some("one"), Some(x)) => 1;
    case Choice(Some("two"), None) => 1;
    case Choice(y, Some("two")) => 2;
    case Choice(Some(z), a) => 3;
    case Some(b) => 4;
    case None => 5;
  }
}
