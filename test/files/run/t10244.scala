class NotSerializable { def foo = "bar" }

// transient lazy val gets transient bitmap, is initialized after deserializing,
// regardless of whether it was initialized before serializing
trait HasUnserializableLazy extends Serializable {
  @transient
  protected lazy val notSerializable = new NotSerializable
}

class Serializes extends HasUnserializableLazy {
  def check = notSerializable.foo == "bar"
}

object SerializeHelpers {
  def serialize[A](o: A): Array[Byte] = {
    val ba = new java.io.ByteArrayOutputStream(512)
    val out = new java.io.ObjectOutputStream(ba)
    out.writeObject(o)
    out.close()
    ba.toByteArray()
  }
  def deserialize[A](buffer: Array[Byte]): A = {
    val in =
      new java.io.ObjectInputStream(new java.io.ByteArrayInputStream(buffer))
    in.readObject().asInstanceOf[A]
  }
}

object Test {
  import SerializeHelpers._

  def main(args: Array[String]): Unit = {
    assert(deserialize[Serializes](serialize(new Serializes)).check)

    // check that transient lazy val uses a transient bitmap,
    // so that it doesn't care whether the lazy val was initialized before serialization or not
    assert(deserialize[Serializes](serialize { val i = new Serializes ; i.check ; i }).check)
  }
}
