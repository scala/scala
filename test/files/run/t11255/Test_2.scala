//> using options -opt:inline:**
object Test {
  def serializeDeserialize(obj: Object): Object = {
    import java.io._
    val buffer = new ByteArrayOutputStream
    val out = new ObjectOutputStream(buffer)
    out.writeObject(obj)
    val in = new ObjectInputStream(new ByteArrayInputStream(buffer.toByteArray))
    in.readObject
  }

  def main(args: Array[String]): Unit = {
    assert(serializeDeserialize((new A).f).asInstanceOf[K].f(10) == 11)
  }
}
