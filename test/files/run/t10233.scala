import java.io.{ByteArrayOutputStream, NotSerializableException, ObjectOutputStream}

object Test {
  def plus(x: Int): Int = x + 1
  def notSerialize(name: String, fn: Int => Int): Unit = try {
    val oos = new ObjectOutputStream(new ByteArrayOutputStream)
    oos.writeObject(fn)
    assert(false)
  } catch {
    case e: NotSerializableException =>
      // expected
  }
  def serialize(name: String, fn: Int => Int): Unit = {
    try {
      val oos = new ObjectOutputStream(new ByteArrayOutputStream)
      oos.writeObject(fn)
    } catch {
      case e: NotSerializableException =>
        println(s"NotSerializableException: $name")
        e.printStackTrace()
    }
  }
  object Inner {
    def minus(x: Int): Int = x - 1 
    def testInner(): Unit = {
      serialize("plus", plus)
      serialize("this.plus", Test.this.plus)
      serialize("Test.plus", Test.plus)

      serialize("minus", minus)
      serialize("this.minus", this.minus)
      serialize("Inner.minus", Inner.minus)
    }
    def testLocal(): Unit = {
     object Local {
        def zero(x: Int) = 0
        def apply(): Unit = {
          serialize("plus", plus)
          serialize("this.plus", Test.this.plus)
          serialize("Test.plus", Test.plus)

          serialize("minus", minus)
          serialize("Inner.minus", Inner.minus)

          notSerialize("zero", zero)
          notSerialize("this.zero", this.zero)
          notSerialize("Local.zero", Local.zero)
        }
      }
      Local()
    }
  }
  def main(args: Array[String]): Unit = {
    serialize("plus", plus)
    serialize("this.plus", this.plus)
    serialize("Test.plus", Test.plus)

    Inner.testInner()

    Inner.testLocal()
  }
}
