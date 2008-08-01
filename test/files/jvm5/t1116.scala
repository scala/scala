object Serialize {
  @throws(classOf[java.io.IOException])
  def write[A](o: A): Array[Byte] = {
    val ba = new java.io.ByteArrayOutputStream(512)
    val out = new java.io.ObjectOutputStream(ba)
    out.writeObject(o)
    out.close()
    ba.toByteArray()
  }
  @throws(classOf[java.io.IOException])
  @throws(classOf[ClassNotFoundException])
  def read[A](buffer: Array[Byte]): A = {
    val in =
      new java.io.ObjectInputStream(new java.io.ByteArrayInputStream(buffer))
    in.readObject().asInstanceOf[A]
  }
}

object Foo {
  def obj_foo(x: Int) = { () => x }
}

object Test {
  def main(args: Array[String]) {
    Serialize.write(Foo.obj_foo(3))
  }
}
