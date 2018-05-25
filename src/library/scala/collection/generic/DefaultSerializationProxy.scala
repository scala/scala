package scala.collection.generic

import java.io.{ObjectInputStream, ObjectOutputStream}

import scala.collection.{Factory, Iterable}
import scala.collection.mutable.Builder

/** The default serialization proxy for collection implementations.
  *
  * This class is `final` and requires an extra `Factory` object rather than leaving the details of creating a `Builder`
  * to an abstract method that could be implemented by a subclass. This is necessary because the factory is needed
  * for deserializing this class's private state, which happens before any subclass fields would be deserialized. Any
  * additional state required to create the proper `Builder` needs to be captured by the `factory`.
  */
@SerialVersionUID(3L)
final class DefaultSerializationProxy[A](factory: Factory[A, Any], @transient private[this] val coll: Iterable[A]) extends Serializable {

  @transient protected var builder: Builder[A, Any] = _

  private[this] def writeObject(out: ObjectOutputStream): Unit = {
    out.defaultWriteObject()
    val k = coll.knownSize
    out.writeInt(k)
    var count = 0
    coll.foreach { x =>
      out.writeObject(x)
      count += 1
    }
    if(k >= 0) {
      if(count != k) throw new IllegalStateException(s"Illegal size $count of collection, expected $k")
    } else out.writeObject(SerializeEnd)
  }

  private[this] def readObject(in: ObjectInputStream): Unit = {
    in.defaultReadObject()
    builder = factory.newBuilder
    val k = in.readInt()
    if(k >= 0) {
      builder.sizeHint(k)
      var count = 0
      while(count < k) {
        builder += in.readObject().asInstanceOf[A]
        count += 1
      }
    } else {
      while (true) in.readObject match {
        case SerializeEnd => return
        case a => builder += a.asInstanceOf[A]
      }
    }
  }

  protected[this] def readResolve(): Any = builder.result()
}

@SerialVersionUID(3L)
private[collection] case object SerializeEnd
