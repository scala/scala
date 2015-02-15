
trait TheOldCollegeTry {

  // was: value isDefinedAt is not a member of Int
  // now: required: Function[Throwable,?]
  def f = try ??? catch 22

  def g = try 42

  def h = List("x") map (s => try { case _ => 7 })

  def j = try ??? catch (_ => 42)

  import PartialFunction.fromFunction

  def recover(t: Throwable): Int = 42
  def k  = try 27 catch fromFunction(recover)
  def k2 = try 27 catch recover

  def parseErrorHandler[T]: PartialFunction[Throwable, T] = ???
  def pushBusy[T](body: => T): T =
    try body
    catch parseErrorHandler

  object catcher {
    def isDefinedAt(x: Any) = true
    def apply(x: Any) = 27
  }
  def noLongerAllower: Int = try 42 catch catcher
}
