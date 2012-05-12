/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2008-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.util

/**
 * Marshalling of Scala objects using Scala tags.
 *
 * @author Stephane Micheloud
 * @version 1.0
 */
@deprecated("This class will be removed", "2.10.0")
object Marshal {
  import java.io._
  import scala.reflect.ClassTag

  def dump[A](o: A)(implicit t: ClassTag[A]): Array[Byte] = {
    val ba = new ByteArrayOutputStream(512)
    val out = new ObjectOutputStream(ba)
    out.writeObject(t)
    out.writeObject(o)
    out.close()
    ba.toByteArray()
  }

  @throws(classOf[IOException])
  @throws(classOf[ClassCastException])
  @throws(classOf[ClassNotFoundException])
  def load[A](buffer: Array[Byte])(implicit expected: ClassTag[A]): A = {
    val in = new ObjectInputStream(new ByteArrayInputStream(buffer))
    val found = in.readObject.asInstanceOf[ClassTag[_]]
    try {
      // [Eugene] needs review
      // previously was: found <:< expected
      found.erasure.asSubclass(expected.erasure)
      in.readObject.asInstanceOf[A]
    } catch {
      case _: ClassCastException =>
        in.close()
        throw new ClassCastException("type mismatch;"+
          "\n found   : "+found+
          "\n required: "+expected)
    }
  }
}
