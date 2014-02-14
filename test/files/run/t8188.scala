object Test {
  def main(args: Array[String]) {
    import java.io.ByteArrayInputStream
    import java.io.ByteArrayOutputStream
    import java.io.ObjectInputStream
    import java.io.ObjectOutputStream
    import scala.collection.concurrent.TrieMap
   
    def ser[T](o: T): Array[Byte] = {
      val baos = new ByteArrayOutputStream()
      new ObjectOutputStream(baos).writeObject(o)
      baos.toByteArray()
    }

    def deser[T](bs: Array[Byte]): T =
      new ObjectInputStream(new ByteArrayInputStream(bs)).readObject().asInstanceOf[T]
   
    def cloneViaSerialization[T](t: T): T = deser(ser(t))
   
    val f = cloneViaSerialization(_: TrieMap[Int, Int])
    val tm = TrieMap(1 -> 2)
    assert( f(f(tm)) == tm )
    assert( ser(tm).length == ser(f(tm)).length )
  }
}
