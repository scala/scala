/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.actors
package remote

import java.io.{ByteArrayInputStream, ByteArrayOutputStream,
                ObjectInputStream, ObjectOutputStream, InputStream,
                ObjectStreamClass}

/**
 *  @author Guy Oliver
 */
private[remote] class CustomObjectInputStream(in: InputStream, cl: ClassLoader)
extends ObjectInputStream(in) {
  override def resolveClass(cd: ObjectStreamClass): Class[_] =
    try {
      cl.loadClass(cd.getName())
    } catch {
      case cnf: ClassNotFoundException =>
        super.resolveClass(cd)
    }
  override def resolveProxyClass(interfaces: Array[String]): Class[_] =
    try {
      val ifaces = interfaces map { iface => cl.loadClass(iface) }
      java.lang.reflect.Proxy.getProxyClass(cl, ifaces: _*)
    } catch {
      case e: ClassNotFoundException =>
        super.resolveProxyClass(interfaces)
    }
}

/**
 *  @author Philipp Haller
 */
class JavaSerializer(serv: Service, cl: ClassLoader) extends Serializer(serv) {
  def serialize(o: AnyRef): Array[Byte] = {
    val bos = new ByteArrayOutputStream()
    val out = new ObjectOutputStream(bos)
    out.writeObject(o)
    out.flush()
    bos.toByteArray()
  }

  def deserialize(bytes: Array[Byte]): AnyRef = {
    val bis = new ByteArrayInputStream(bytes)

    // use custom stream only if cl != null
    val in = if (cl != null)
      new CustomObjectInputStream(bis, cl)
    else
      new ObjectInputStream(bis)

    in.readObject()
  }
}
