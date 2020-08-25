// scalac: -Ydelambdafy:method-ref
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.io.{ObjectInputStream, ObjectOutputStream}

object Test {
  def main(args: Array[String]): Unit = {
    roundTripMethRef()
    roundTripMethRef_F0()
  }

  // lambda targeting a method reference, not a SAM or FunctionN (should behave the same way)
  def roundTripMethRef(): Unit = {
    val lambda: String => String = (s: String) => s.toUpperCase
    val reconstituted1 = serializeDeserialize(lambda).asInstanceOf[String => String]
    val reconstituted2 = serializeDeserialize(reconstituted1).asInstanceOf[String => String]
    assert(reconstituted1.apply("yo") == "YO")
    assert(reconstituted1.getClass == reconstituted2.getClass)
  }

  def name = "Test"

  def roundTripMethRef_F0(): Unit = {
    val lambda: () => String = () => Test.name
    val reconstituted1 = serializeDeserialize(lambda).asInstanceOf[() => String]
    val reconstituted2 = serializeDeserialize(reconstituted1).asInstanceOf[() => String]
    assert(reconstituted1.apply() == "Test")
    assert(reconstituted1.getClass == reconstituted2.getClass)
  }

  def serializeDeserialize[T <: AnyRef](obj: T) = {
    val buffer = new ByteArrayOutputStream
    val out = new ObjectOutputStream(buffer)
    out.writeObject(obj)
    val in = new ObjectInputStream(new ByteArrayInputStream(buffer.toByteArray))
    in.readObject.asInstanceOf[T]
  }
}
