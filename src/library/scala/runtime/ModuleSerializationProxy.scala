package scala.runtime

/** A serialization proxy for singleton objects */
@SerialVersionUID(1L)
final class ModuleSerializationProxy(cls: Class[_]) extends Serializable {
  private[this] def readResolve: AnyRef = cls.getField("MODULE$").get(null)
}
