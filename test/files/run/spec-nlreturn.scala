//scalac: -Xlint:-nonlocal-return
object Test {
  def f(): Int = {
    try (1 to 10).map { i => return 16 ; i }.sum
    catch {
      case x: runtime.NonLocalReturnControl[_] =>
        assert(x.getClass.getName == "scala.runtime.NonLocalReturnControl$mcI$sp")
        x.value.asInstanceOf[Int]
    }
  }

  def main(args: Array[String]): Unit = assert(f() == 16)
}
