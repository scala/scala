//> using options -Xmixin-force-forwarders:false

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}

trait T1 extends Serializable {
  def writeReplace(): AnyRef = new SerializationProxy(this.asInstanceOf[C].s)
}
trait T2 {
  def readResolve: AnyRef = new C(this.asInstanceOf[SerializationProxy].s.toLowerCase)
}
class C(val s: String) extends T1
class SerializationProxy(val s: String) extends T2 with Serializable

object Test {
  def serializeDeserialize[T <: AnyRef](obj: T) = {
    val buffer = new ByteArrayOutputStream
    val out = new ObjectOutputStream(buffer)
    out.writeObject(obj)
    val in = new ObjectInputStream(new ByteArrayInputStream(buffer.toByteArray))
    in.readObject.asInstanceOf[T]
  }

  def main(args: Array[String]): Unit = {
    val c1 = new C("TEXT")
    val c2 = serializeDeserialize(c1)
    assert(c2.s == "text")
  }
}
