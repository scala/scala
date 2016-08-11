trait T {
  val abs: String
  protected val protabs: String
  val pub = "public"
  protected val prot = "protected"
  private val privvy = "private"
  private[this] val privateThis = "private[this]"
  // TODO:
  // final val const = "const"

  trait Nested { println(abs + privateThis) }

  object NO {
    println(abs)
    println(pub)
    println(prot)
    println(protabs)
    println(privvy)
    println(privateThis)
  }

  trait NT {
    println(abs)
    println(pub)
    println(prot)
    println(protabs)
    println(privvy)
    println(privateThis)
  }

  class NC {
    println(abs)
    println(pub)
    println(prot)
    println(protabs)
    println(privvy)
    println(privateThis)
  }
}

class C extends AnyRef with T {
  println("x")
  val abs = "abstract"
  println("y")
  val protabs = "abstract protected"
  final val const = "const"
  println("z")
}

object Test extends C {
  def main(args: Array[String]): Unit = {
  NO
  new NT{}
  new NC
}}