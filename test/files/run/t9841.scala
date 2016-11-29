
class Container {
  private case class Inner(s: String)
  private object Inner {
    val Empty = Inner("")
  }
  private val state = Inner.Empty
}

object Test extends App {
  new Container
}
