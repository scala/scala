trait Option[+a] {}

case class Some[a](x: a) extends Option[a] {
  override def toString(): String = "Some(" + x + ")"
  override def equals(that: Any): Boolean = that match {
    case Some(x) => this.x == x
    case _ => false
  }
  override def hashCode(): Int = getClass().hashCode() * 41 + x.hashCode()
}

case object None extends Option[Nothing] {
  override def toString(): String = "None"
  override def equals(that: Any) = that match {
    case None => true
    case _ => false
  }
  override def hashCode(): Int = getClass().hashCode()
}

object test {

  def println(str: String): Unit = java.lang.System.out.println(str)

  def print(opt: Option[String]) = opt match {
    case Some(x) => println(x)
    case None => println("nothing")
  }
}

// if bodies are duplicated, then we would get an error like "double definition"

trait John[A,B] {
  def filter(x:Any) =    x match {
    case (x::xs, _) => "ga"
    case _ => {x:String => "foobar"}
  }
}
