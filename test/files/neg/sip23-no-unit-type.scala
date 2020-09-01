
object Test {
  def f: () => Int = () => 42
  def s = null.asInstanceOf[Any] match {
    case ()         => "ok"
    case _: ()      => "err"
    case _: ().type => "err"
    case _: Unit    => "quite good"
  }
  val younit: () = ()
  val unit: ().type = ()
}
