class Test(x: => Object) extends Serializable {
  @transient lazy val foo = x
}

object Test {
  def main(args: Array[String]): Unit = {
    import java.io._
    val t = new Test("foo")
    println(t.foo)
    val baos = new ByteArrayOutputStream
    val dos = new ObjectOutputStream(baos)
    dos.writeObject(t)
    dos.close()
    val dis = new ObjectInputStream(new ByteArrayInputStream(baos.toByteArray()))
    val t1 = dis.readObject().asInstanceOf[Test]
    println(t1.foo) // was NPE
  }
}
