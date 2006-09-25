
package nactors.distributed

import java.io.{DataInputStream,DataOutputStream,EOFException}
import scala.io.BytePickle.SPU

abstract class Serializer(val service: Service) {
  def serialize(o: AnyRef/*, w: Writer*/): Array[byte]
  def deserialize(a: Array[byte]/*r: Reader*/): AnyRef

  // throws IOException
  def readBytes(inputStream: DataInputStream): Array[byte] = {
    try {
      val length = inputStream.readInt()
      val bytes = new Array[byte](length)
      inputStream.readFully(bytes, 0, length)
      return bytes
    }
    catch {
      case npe: NullPointerException =>
        throw new EOFException("Connection closed.")
    }
  }

  // throws IOException, ClassNotFoundException
  def readObject(inputStream: DataInputStream): AnyRef = {
    val bytes = readBytes(inputStream)
    deserialize(bytes)
  }

  // throws IOException
  def writeBytes(outputStream: DataOutputStream, bytes: Array[byte]): unit = {
    val length = bytes.length;
    // original length
    outputStream.writeInt(length)
    outputStream.write(bytes, 0, length)
    outputStream.flush()
  }

  // throws IOException
  def writeObject(outputStream: DataOutputStream, obj: AnyRef) = {
    val bytes = serialize(obj)
    writeBytes(outputStream, bytes)
  }
}
