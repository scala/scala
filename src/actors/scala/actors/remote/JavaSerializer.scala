/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors.remote

import java.io.{ByteArrayInputStream, ByteArrayOutputStream,
                ObjectInputStream, ObjectOutputStream, InputStream,
                ObjectStreamClass}

/**
 *  @author Guy Oliver
 */
class CustomObjectInputStream(os: InputStream, val cl: ClassLoader) extends ObjectInputStream(os) {
  override def resolveClass(cd: ObjectStreamClass): Class[T] forSome { type T } =
    try {
      cl.loadClass(cd.getName())
    } catch {
      case cnf: ClassNotFoundException =>
        super.resolveClass(cd)
    }
}

/**
 *  @author Philipp Haller
 */
class JavaSerializer(serv: Service, val cl: ClassLoader) extends Serializer(serv) {
  def serialize(o: AnyRef): Array[Byte] = {
    val bos = new ByteArrayOutputStream()
    val out = new ObjectOutputStream(bos)
    out.writeObject(o)
    out.flush()
    bos.toByteArray()
  }

  def deserialize(bytes: Array[Byte]): AnyRef = {
    val bis = new ByteArrayInputStream(bytes)
    val in = new CustomObjectInputStream(bis, cl)
    in.readObject()
  }
}
