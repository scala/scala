trait Option[a] {}
case class Some[a](x: a) extends Option[a];
case class None[a]() extends Option[a];

module test {

  def println(str: String): Unit = java.lang.System.out.println(str);

  def print(opt: Option[String]) = opt match {
    case Some(x) => println(x);
    case None() => println("nothing");
  }
}