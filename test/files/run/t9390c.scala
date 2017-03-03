class C { // C is not serializable
  def foo = {
    { (x: Any) => new Object {} }
  }
}
object Test {
  def main(args: Array[String]): Unit = {
    val c = new C
    val f = c.foo
    val f1 = serializeDeserialize(f)
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
