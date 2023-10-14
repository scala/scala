trait QuitePrivate[@specialized(Int) K] {
  protected[this] def hasK(k :K) :Boolean
}

trait MoreOpen extends QuitePrivate[Int] {
  override def hasK(k :Int) :Boolean
}


object Playground extends App {
  def check(guy :MoreOpen) = guy.hasK(42)
}