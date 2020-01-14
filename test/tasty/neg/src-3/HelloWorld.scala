package helloworld

object HelloWorld {
  inline val msg: String = "Hello, World!"
  final val msg1 = "Hello, World!"
  def acceptsOnlyMsg1(m: msg1.type): String = m + m
}
