object Test {
  trait GenericBase[T] {
    def init: T
    val value: T = init
    def get: T = value
  }

  class SpecializedClass[@specialized(Double)T](x: T) extends GenericBase[T] {
    override def init: T = x
  }

  def main(args: Array[String]): Unit = {
    val x = new SpecializedClass(1.0)
    println(x.get)
  }
}