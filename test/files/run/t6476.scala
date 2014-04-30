// move to junit after -Xfuture arrives
object Test extends App {
  val x = 0
  assert(s"$"$x$""     == """"0"""")
  assert(s"""$"$x$"""" == """"0"""")
  /*
  desert(s«"$x"»       == """"0"""")
  */
}
