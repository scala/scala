import java.io.{ByteArrayInputStream, ObjectInputStream, ObjectOutputStream, ByteArrayOutputStream}

trait IntToString extends java.io.Serializable { def apply(i: Int): String }

object Test {
  def main(args: Array[String]): Unit = {
    roundTrip()
    roundTripIndySam()
  }

  def roundTrip(): Unit = {
    val c = new Capture("Capture")
    val lambda = (p: Param) => ("a", p, c)
    val reconstituted1 = serializeDeserialize(lambda).asInstanceOf[Object => Any]
    val p = new Param
    assert(reconstituted1.apply(p) == ("a", p, c))
    val reconstituted2 = serializeDeserialize(lambda).asInstanceOf[Object => Any]
    assert(reconstituted1.getClass == reconstituted2.getClass)

    val reconstituted3 = serializeDeserialize(reconstituted1)
    assert(reconstituted3.apply(p) == ("a", p, c))

    val specializedLambda = (p: Int) => List(p, c).length
    assert(serializeDeserialize(specializedLambda).apply(42) == 2)
    assert(serializeDeserialize(serializeDeserialize(specializedLambda)).apply(42) == 2)
  }

  // lambda targeting a SAM, not a FunctionN (should behave the same way)
  def roundTripIndySam(): Unit = {
    val lambda: IntToString = (x: Int) => "yo!" * x
    val reconstituted1 = serializeDeserialize(lambda).asInstanceOf[IntToString]
    val reconstituted2 = serializeDeserialize(reconstituted1).asInstanceOf[IntToString]
    assert(reconstituted1.apply(2) == "yo!yo!")
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

case class Capture(s: String) extends Serializable
class Param
