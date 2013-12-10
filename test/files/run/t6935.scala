object Test {

  def main(args: Array[String]): Unit = {
    import java.io._
    val bytes = new ByteArrayOutputStream()
    val out = new ObjectOutputStream(bytes)
    out.writeObject(())
    out.close()
    val buf = bytes.toByteArray
    val in = new ObjectInputStream(new ByteArrayInputStream(buf))
    val unit = in.readObject()
    assert(unit == ())
  }
}
