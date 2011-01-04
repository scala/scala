
// see ticket #3869
object Test {
  def f: Unit =
    try return finally while(true) ()

  def main(args: Array[String]) {
    f
  }
}
