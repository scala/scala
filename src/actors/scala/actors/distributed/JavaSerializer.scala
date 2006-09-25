/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors.distributed

import java.io._
import scala.io.BytePickle.SPU

[serializable]
class JavaSerializer(serv: Service) extends Serializer(serv) {
  val debug = true

  def log (s: String) =
    if (debug) scala.Console.println("JAVASerializer: " + s)

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

  def pid: SPU[RemotePid] = null
  def addRep(name: String, repCons: Serializer => AnyRef): Unit = {}
}
