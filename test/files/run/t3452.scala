trait IStringPair[T] {
  def a : String
  def b : String
  def build(a : String, b : String) : T
  def cat(that : IStringPair[T]) = build(this.a + that.a, this.b + that.b)
  override def toString = a + b
}

class StringPair(val a : String, val b : String) extends IStringPair[StringPair] {
  def build(a : String, b : String) = new StringPair(a, b)
  def len = a.length + b.length
}

object Test {
  def main(args: Array[String]): Unit = {
    val a = new StringPair("A", "B")
    val b = new StringPair("1", "2")
    val c = a cat b
    println(c.len)
  }
}
