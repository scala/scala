import java.io._

trait NotSerializableInterface { def apply(a: Any): Any }
abstract class NotSerializableClass { def apply(a: Any): Any }
// SAM type that supports lambdas-as-invoke-dynamic
trait IsSerializableInterface extends java.io.Serializable { def apply(a: Any): Any }
// SAM type that still requires lambdas-as-anonhmous-classes
abstract class IsSerializableClass extends java.io.Serializable { def apply(a: Any): Any }

object Test {
  def main(args: Array[String]) {
    val nsi: NotSerializableInterface = x => x
    val nsc: NotSerializableClass = x => x

    import SerDes._
    assertNotSerializable(nsi)
    assertNotSerializable(nsc)
    assert(serializeDeserialize[IsSerializableInterface](x => x).apply("foo") == "foo")
    assert(serializeDeserialize[IsSerializableClass](x => x).apply("foo") == "foo")
    assert(ObjectStreamClass.lookup(((x => x): IsSerializableClass).getClass).getSerialVersionUID == 0)
  }
}

object SerDes {
  def assertNotSerializable(a: AnyRef): Unit = {
    try {
      serialize(a)
      assert(false)
    } catch {
      case _: NotSerializableException => // okay
    }
  }

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
}
