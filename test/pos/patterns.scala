trait Option[a] {}
case class Some[a](x: a) extends Option[a] {
  override def toString(): scala.String = "Some(" + x + ")";
  override def == (that: Any): Boolean = that match {
    case Some(x) => this.x == x
    case _ => false
  }
  override def hashCode(): scala.Int = getClass().hashCode() * 41 + x.hashCode();
}
case class None[a] extends Option[a] {
  override def toString(): scala.String = "None";
  override def == (that: Any) = that match {
    case None => true
    case _ => false
  }
  override def hashCode(): scala.Int = getClass().hashCode();
}

object test {

  def println(str: String): Unit = java.lang.System.out.println(str);

  def print(opt: Option[String]) = opt match {
    case Some(x) => println(x);
    case None => println("nothing");
  }
}