object Test {
  def serializeDeserialize[T <: AnyRef](obj: T): T = {
    import java.io._
    val buffer = new ByteArrayOutputStream
    val out = new ObjectOutputStream(buffer)
    out.writeObject(obj)
    val in = new ObjectInputStream(new ByteArrayInputStream(buffer.toByteArray))
    in.readObject.asInstanceOf[T]
  }

  def gi: () => Int = {
    lazy val x = { println("gi init x"); 1 }
    serializeDeserialize(() => x)
  }

  def gs: () => String = {
    lazy val x = { println("gs init x"); "hi" }
    serializeDeserialize(() => x)
  }

  def main(args: Array[String]): Unit = {
    val fi1 = gi
    println(fi1())
    println(fi1())

    val fi2 = gi
    println(fi2())
    println(fi2())

    val fs1 = gs
    println(fs1())
    println(fs1())

    val fs2 = gs
    println(fs2())
    println(fs2())
  }
}