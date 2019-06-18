
trait TheOldCollegeTry {

  // was: value isDefinedAt is not a member of Int
  // now: required: PartialFunction[Throwable,?]
  //def f = try ??? catch 22

  def g = try 42

  def h = List("x") map (s => try { case _ => 7 })
}
