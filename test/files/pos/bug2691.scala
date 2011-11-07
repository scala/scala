object Breakdown {
  def unapplySeq(x: Int): Some[List[String]] = Some(List("", "there")) 
}
object Test {
  42 match {
    case Breakdown("") =>  // needed to trigger bug
    case Breakdown("foo") =>  // needed to trigger bug
    case Breakdown("", who) => println ("hello " + who)
  }
}
