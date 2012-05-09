
class OverTheTop {
  def info0(m: String) = m
  def info0(m: String, args: Any*) = m +" "+ args.mkString(" ")

  // as reported
  def info1(m: =>String) = m
  def info1(m: =>String, args: Any*) = m +" "+ args.mkString(", ")

  def mixed(m: String) = m + "!"
  def mixed(m: =>String, args: Any*) = m +" "+ args.mkString(" ") + "?"
  def mixed(m: =>String) = "gee " + m + "!"

  def goForIt(m: =>String) = mixed(m)  // go for the eval param!
}
class Tri {
  def choices(m: =>String,n: =>String) = m +" "+ n + "!"
  def choices(m: =>String,n: String) = m +" "+ n + "!!"
  def choices(m: String,n: =>String) = m +" "+ n + "!!!"
  def choices(m: String,n: String) = m +" "+ n + "!!!!"

  def disambiguate(m: String,n: =>String) = choices(m,n)
}

object Test {
  def main(args: Array[String]) {
    val top = new OverTheTop
    println(top.info0("hello"))
    println(top.info0("hello","working","world"))
    println(top.info1("goodnight"))
    println(top.info1("goodnight", "moon", "nobody", "noises everywhere"))
    println(top.mixed("knock-knock"))
    println(top.mixed("who's", "there"))
    println(top.goForIt("whiz"))
    val tri = new Tri
    println(tri.choices("resolve","this"))
    println(tri.disambiguate("hello","world"))
  }
}
