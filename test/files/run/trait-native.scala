trait T {
  @native def foo = ???
}
object Test extends T {
  def main(args: Array[String]): Unit = {
     // was VerifyError
  }
}
