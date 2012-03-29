// shouldn't result in a verify error when run...
object Test extends App {
  @annotation.tailrec
  final def test(meh: Boolean): Boolean = {
    Some("a") match {
      case x =>
        x match {
          case Some(_) => if(meh) test(false) else false
          case _ => test(false)
        }
    }
  }
  println(test(true))
}