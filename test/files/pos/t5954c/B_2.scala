package object A {
  // these used to should be prevented by the implementation restriction
  // but are now allowed
  class B
  object B
  trait C
  object C
  case class D()
  // all the rest of these should be ok
  class E
  object F
  val g = "omg"
  var h = "wtf"
  def i = "lol"
  type j = String
  class K(val k : Int) extends AnyVal
  implicit class L(val l : Int)
}
