// SI-9841 regrettable behavior initializing private inner object
// A fix is not yet planned for 2.11.9, but it works in 2.12.x.
//
//at Container.Container$$Inner$lzycompute(t9841.scala:4)
//at Container.Container$$Inner(t9841.scala:4)
//at Container$Inner$.<init>(t9841.scala:5)
//
class Container {
  private case class Inner(s: String)
  private object Inner {
    val Empty = Inner("")
  }
  private val state = Inner.Empty
}

object Test extends App {
  val catcher: PartialFunction[Throwable, Unit] = {
    case _: StackOverflowError =>
  }
  try {
    new Container
    Console println "Expected StackOverflowError"
  } catch catcher
}
