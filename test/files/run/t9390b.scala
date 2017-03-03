class C { // C is not serializable
  def foo = (x: Int) => (y: Int) => x + y
  def bar = (x: Int) => (y: Int) => {toString; x + y}
}

object Test {
  def main(args: Array[String]): Unit = {
    val c = new C
    val f = c.foo
    assert(f(1)(2) == 3)
    val f1 = serializeDeserialize(f)
    assert(f1(1)(2) == 3)

    try {
      serializeDeserialize(c.bar)
      assert(false)
    } catch {
      case _: java.io.NotSerializableException =>
        // expected, lambda transitively refers to this
    }
  }

  def serializeDeserialize[T <: AnyRef](obj: T): T = {
    import java.io._
    val buffer = new ByteArrayOutputStream
    val out = new ObjectOutputStream(buffer)
    out.writeObject(obj)
    val in = new ObjectInputStream(new ByteArrayInputStream(buffer.toByteArray))
    in.readObject.asInstanceOf[T]
  }
}
