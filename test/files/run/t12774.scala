trait SerializableBiFunction[T, U, R] extends java.util.function.BiFunction[T, U, R] with Serializable {
  // def apply(t: T, u: U): R
}
object Test {
  def main(args: Array[String]): Unit = {
    import java.io._
    val fn: SerializableBiFunction[String, Int, Boolean] = (str, expected) => str.length == expected

    val buffer = new ByteArrayOutputStream
    val out = new ObjectOutputStream(buffer)
    out.writeObject(fn)
    val in = new ObjectInputStream(new ByteArrayInputStream(buffer.toByteArray))
    val res = in.readObject.asInstanceOf[SerializableBiFunction[String, Int, Boolean]]
    assert(res("success", 7))
  }
}
