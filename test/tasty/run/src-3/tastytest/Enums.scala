package tastytest

import java.util.HashMap
import java.lang.ClassValue
import scala.reflect.ClassTag

object Enums {

  abstract class Enum[E <: Enum[E]: EnumCompanion: ClassTag] protected (val name: String, val ordinal: Int)
  extends Ordered[E] with Cloneable {
    Enum.register(this)

    final override def equals(that: Any): Boolean = that.asInstanceOf[AnyRef] `eq` this
    final override def hashCode: Int = super.hashCode

    final def compare(that: E): Int = ordinal - that.ordinal

    override def toString: String = name
    protected final override def clone(): AnyRef = throw new CloneNotSupportedException()
    protected final override def finalize(): Unit = ()
  }

  abstract class EnumCompanion[E <: Enum[E]: ClassTag] { self =>
    private[Enums] val cache: HashMap[String, E] = new HashMap()

    implicit final def visibleCompanion: self.type = self

    def valueOf(name: String): E = cache.get(name) match {
      case null => throw new IllegalArgumentException(name)
      case some => some
    }

    def values: Array[E] = cache.values.toArray(Array.ofDim[E](cache.size)).sorted
  }

  object Enum {

    private final def register[E <: Enum[E]: ClassTag](value: Enum[E])(implicit companion: EnumCompanion[E]): Unit = value match {
      case value: E =>
        if (companion.cache.containsKey(value.name))
          throw new IllegalArgumentException(s"${implicitly[ClassTag[E]]}.${value.name} has already been defined.")
        else
          companion.cache.put(value.name, value)

      case _ => throw new IllegalArgumentException(s"${value.getClass} is not ${implicitly[ClassTag[E]]}.")
    }

    def valueOf[E <: Enum[E]: EnumCompanion](name: String) = implicitly[EnumCompanion[E]].valueOf(name)
    def values[E <: Enum[E]: EnumCompanion]: Array[E] = implicitly[EnumCompanion[E]].values
  }
}
