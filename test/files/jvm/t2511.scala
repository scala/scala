object Test {

  import java.io._

  trait IMyMessage extends Serializable {
    @transient var message: String = null
    var message2: String = null
  }

  class MyMessage extends IMyMessage

  def serialize = {
    val buf = new ByteArrayOutputStream(10000)
    val out = new ObjectOutputStream(buf)
    val m = new MyMessage
    m.message = "foo"
    m.message2 = "bippy"
    out.writeObject(m)
    out.flush
    buf.toByteArray
  }

  def unserialize(buf:Array[Byte]) = {
    val in = new ObjectInputStream(new ByteArrayInputStream(buf))
    in.readObject.asInstanceOf[MyMessage]
  }

  def main(args: Array[String]) {
    val m = unserialize(serialize)
    // Xcheckinit freaks out here but its nullness is what we're testing
    try println(m.message)
    catch { case _: UninitializedFieldError => println("null") }
    println(m.message2)
  }
}
