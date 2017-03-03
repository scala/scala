class C { @inline final def f: Int => Int = (x: Int) => x + 1 }

object Test extends App {
  import java.io._

  def serialize(obj: AnyRef): Array[Byte] = {
    val buffer = new ByteArrayOutputStream
    val out = new ObjectOutputStream(buffer)
    out.writeObject(obj)
    buffer.toByteArray
  }
  def deserialize(a: Array[Byte]): AnyRef = {
    val in = new ObjectInputStream(new ByteArrayInputStream(a))
    in.readObject
  }

  def serializeDeserialize[T <: AnyRef](obj: T) = deserialize(serialize(obj)).asInstanceOf[T]

  assert(serializeDeserialize((new C).f).isInstanceOf[Function1[_, _]])
}
