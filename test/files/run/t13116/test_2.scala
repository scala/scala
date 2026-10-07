//> using options -Werror -Xlint

object Test extends App {
  val expect: ExpectMacro = null
  implicit val loc: SourceLocation = SourceLocation("testloc")

  val x = 27
  println {
    expect(s"$x" == "42")
  }
}
