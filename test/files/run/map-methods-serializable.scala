import java.io._
import collection._

/** Tests fixes for SI-5018, SI-6654, and SI-7005. These are all in a single test
  * to avoid repeating the serializeDeserialize method, and because the tests all
  * have to do with the serializability of objects returned from Map methods.
  */
object Test {

  def main(args: Array[String]) {
    // SI-5018 Map.WithDefault is not serializable 
    val values = mutable.Map(1 -> 1).values
    assert(serializeDeserialize(values).toList == values.toList)

    assertSerializeDeserialize(mutable.Map(1 -> 1).keySet)
    assertSerializeDeserialize(immutable.Map(1 -> 1).keySet)
    assertSerializeDeserialize(immutable.Map(1 -> 1).withDefaultValue(1))
    assertSerializeDeserialize(mutable.Map(1 -> 1).withDefault(x => -x))

    // SI-6654 Map#filterKeys not serializable
    def isEven(i: Int): Boolean = i % 2 == 0
    assertSerializeDeserialize(mutable.Map(1 -> 1, 2 -> 2) filterKeys isEven)
    assertSerializeDeserialize(immutable.Map(1 -> 1, 2 -> 2) filterKeys isEven)

    // SI-7005  Map#mapValues not serializable
    assertSerializeDeserialize(mutable.Map(1 -> 1, 2 -> 2) mapValues isEven)
    assertSerializeDeserialize(immutable.Map(1 -> 1, 2 -> 2) mapValues isEven)
  }

  def assertSerializeDeserialize[T <: AnyRef](obj: T) = {
    assert(serializeDeserialize(obj) == obj)
  }

  def serializeDeserialize[T <: AnyRef](obj: T) = {
    val buffer = new ByteArrayOutputStream
    val out = new ObjectOutputStream(buffer)
    out.writeObject(obj)
    val in = new ObjectInputStream(new ByteArrayInputStream(buffer.toByteArray))
    in.readObject.asInstanceOf[T]
  }
}
