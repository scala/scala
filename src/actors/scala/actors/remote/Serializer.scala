/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.actors
package remote


import java.lang.ClassNotFoundException

import java.io.{DataInputStream, DataOutputStream, EOFException, IOException}

abstract class Serializer(val service: Service) {
  def serialize(o: AnyRef): Array[Byte]
  def deserialize(a: Array[Byte]): AnyRef

  @throws(classOf[IOException])
  private def readBytes(inputStream: DataInputStream): Array[Byte] = {
    try {
      val length = inputStream.readInt()
      val bytes = new Array[Byte](length)
      inputStream.readFully(bytes, 0, length)
      bytes
    }
    catch {
      case npe: NullPointerException =>
        throw new EOFException("Connection closed.")
    }
  }

  @throws(classOf[IOException]) @throws(classOf[ClassNotFoundException])
  def readObject(inputStream: DataInputStream): AnyRef = {
    val bytes = readBytes(inputStream)
    deserialize(bytes)
  }

  @throws(classOf[IOException])
  private def writeBytes(outputStream: DataOutputStream, bytes: Array[Byte]) {
    val length = bytes.length;
    // original length
    outputStream.writeInt(length)
    outputStream.write(bytes, 0, length)
    outputStream.flush()
  }

  @throws(classOf[IOException])
  def writeObject(outputStream: DataOutputStream, obj: AnyRef) {
    val bytes = serialize(obj)
    writeBytes(outputStream, bytes)
  }
}
