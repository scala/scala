
package nactors.distributed

import java.io._

import scala.io.BytePickle.SPU

[serializable]
class JavaSerializer(serv: Service) extends Serializer(serv) {
  val debug = true

  def log(s: String) =
    if (debug) Console.println("JavaSerializer: " + s)

  def serialize(o: AnyRef): Array[Byte] = {
    val bos = new ByteArrayOutputStream()
    val out = new ObjectOutputStream(bos)
    out.writeObject(o)
    out.flush()
    bos.toByteArray()
  }

  def deserialize(bytes: Array[Byte]): AnyRef = {
    val bis = new ByteArrayInputStream(bytes)
    val in = new ObjectInputStream(bis)
    in.readObject()
  }

  def addRep(name: String, repCons: Serializer => AnyRef): Unit = {}
}
