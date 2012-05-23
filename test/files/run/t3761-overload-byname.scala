
class OverTheTop {
  def info0(m: String) = m + "!"
  def info0(m: String, args: Any*) = m +" "+ args.mkString(" ")

  // as reported
  def info1(m: =>String) = m + "!"
  def info1(m: =>String, args: Any*) = m +" "+ args.mkString(", ")

  // @lrytz
  def m[A](x: => Int) = 0; def m[A](x: => Int, xs: Int*) = 1 
}
object Test {
  def main(args: Array[String]) {
    val top = new OverTheTop
    println(top.info0("hello"))
    println(top.info0("hello","working","world"))
    println(top.info1("goodnight"))
    println(top.info1("goodnight", "moon", "nobody", "noises everywhere"))
    println(top.m(17))
    println(top.m(17,19))
  }
}
