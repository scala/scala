trait Foo {
  @transient protected var load = 1
  @transient protected var a = 12

  protected def init[B](in: java.io.ObjectInputStream) {
    in.defaultReadObject
    load = in.readInt
    val sizea = in.readInt
    a = 12
  }

  protected def serializeTo(out: java.io.ObjectOutputStream) {
    out.defaultWriteObject
    out.writeInt(load)
    out.writeInt(a)
  }
}


@serializable
class Bar extends Foo {
  @transient protected var first: Any = null
  def size = a
  @transient var second: Any = null
    
  def checkMember { first }
    
  private def writeObject(out: java.io.ObjectOutputStream) {
    serializeTo(out)
  }
  
  private def readObject(in: java.io.ObjectInputStream) {
    first = null
    init(in)
  }
}

object Test {
  private def toObject[A](bytes: Array[Byte]): A = {
    val in = new java.io.ObjectInputStream(new java.io.ByteArrayInputStream(bytes))
    in.readObject.asInstanceOf[A]
  }
  
  private def toBytes(o: AnyRef): Array[Byte] = {
    val bos = new java.io.ByteArrayOutputStream
    val out = new java.io.ObjectOutputStream(bos)
    out.writeObject(o)
    out.close
    bos.toByteArray
  }

    
  def main(args: Array[String]) {
    val a1 = new Bar()
    val serialized:Array[Byte] = toBytes(a1)
    val deserialized: Bar = toObject(serialized)
    deserialized.size
    deserialized.checkMember
  }
}
