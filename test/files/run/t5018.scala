


import java.io._
import collection._



object Test {

  def serializeDeserialize[T <: AnyRef](obj: T) = {
    val buffer = new ByteArrayOutputStream
    val out = new ObjectOutputStream(buffer)
    out.writeObject(obj)
    val in = new ObjectInputStream(new ByteArrayInputStream(buffer.toByteArray))
    in.readObject.asInstanceOf[T]
  }

  def main(args: Array[String]) {
    val values = mutable.Map(1 -> 1).values
    assert(serializeDeserialize(values).toList == values.toList)

    val keyset = mutable.Map(1 -> 1).keySet
    assert(serializeDeserialize(keyset) == keyset)

    val imkeyset = immutable.Map(1 -> 1).keySet
    assert(serializeDeserialize(imkeyset) == imkeyset)

    val defaultmap = immutable.Map(1 -> 1).withDefaultValue(1)
    assert(serializeDeserialize(defaultmap) == defaultmap)

    val minusmap = mutable.Map(1 -> 1).withDefault(x => -x)
    assert(serializeDeserialize(minusmap) == minusmap)
  }

}

