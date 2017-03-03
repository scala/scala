class NotSerializable

trait SerializableActually {
  @transient
  lazy val notSerializedTLV: NotSerializable = new NotSerializable

  @transient
  val notSerializedTL: NotSerializable = new NotSerializable

  @transient
  var notSerializedTR: NotSerializable = new NotSerializable
}

class SerializableBecauseTransient extends Serializable with SerializableActually {
  @transient
  lazy val notSerializedLV: NotSerializable = new NotSerializable

  @transient
  val notSerializedL: NotSerializable = new NotSerializable

  @transient
  var notSerializedR: NotSerializable = new NotSerializable
}

// Indirectly check that the @transient annotation on `notSerialized` made it to the underyling field in bytecode.
// If it doesn't, `writeObject` will fail to serialize the field `notSerialized`, because `NotSerializable` is not serializable
object Test {
  def main(args: Array[String]): Unit = {
    val obj = new SerializableBecauseTransient
    // must force, since `null` valued field is serialized regardless of its type
    val forceTLV = obj.notSerializedTLV
    val forceLV  = obj.notSerializedLV
    new java.io.ObjectOutputStream(new java.io.ByteArrayOutputStream) writeObject obj
  }
}
