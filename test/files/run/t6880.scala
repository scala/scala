
class C(private[this] var c: String) { c = "good" ; def f = c }

object Test {
  def main(args: Array[String]) = println {
    new C("bad").f
  }
}
