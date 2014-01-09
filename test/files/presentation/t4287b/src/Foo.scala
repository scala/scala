trait Greeting {
  val name: String
  val msg = "How are you, "+name
}

object Greeting {
  val hello = "hello"
}

class C(i: Int) extends {
  val nameElse = "Bob"
} with Greeting {
  val name = "avc"
  println(i/*#*/)
}