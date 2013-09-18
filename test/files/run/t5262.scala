






object Test {

  def serializationDeserialization(obj : Any) {
    val bos = new java.io.ByteArrayOutputStream()
    val out = new java.io.ObjectOutputStream(bos)
    out.writeObject(obj)

    val arr = bos.toByteArray()
    val in = new java.io.ObjectInputStream(new java.io.ByteArrayInputStream(arr))
    val o = in.readObject()
    println(o)
  }

  def main(args : Array[String]) {
    serializationDeserialization(List(1,2,3,4))
    serializationDeserialization(List(1,2,null,4))
  }

}
