/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2008-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.util

/**
 * Marshalling of Scala objects using Scala manifests.
 *
 * @author Stephane Micheloud
 * @version 1.0
 */
object Marshal {
  import java.io._
  import scala.reflect.ClassManifest

  def dump[A](o: A)(implicit m: ClassManifest[A]): Array[Byte] = {
    val ba = new ByteArrayOutputStream(512)
    val out = new ObjectOutputStream(ba)
    out.writeObject(m)
    out.writeObject(o)
    out.close()
    ba.toByteArray()
  }

  @throws(classOf[IOException])
  @throws(classOf[ClassCastException])
  @throws(classOf[ClassNotFoundException])
  def load[A](buffer: Array[Byte])(implicit expected: ClassManifest[A]): A = {
    val in = new ObjectInputStream(new ByteArrayInputStream(buffer))
    val found = in.readObject.asInstanceOf[ClassManifest[_]]
    if (found <:< expected) {
      val o = in.readObject.asInstanceOf[A]
      in.close()
      o
    } else {
      in.close()
      throw new ClassCastException("type mismatch;"+
        "\n found   : "+found+
        "\n required: "+expected)
    }
  }

}
