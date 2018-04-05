package scala.collection.immutable

/**
  * This class allows to create custom integer values that have a
  * hash code that differs from the value itself.
  *
  * The main use-case for this class is to simulate hash-collisions
  * when testing collection data structures.
  */
class CustomHashInt(val value: Int, val hash: Int) {

  override def hashCode: Int = hash

  override def equals(other: Any): Boolean = other match {
    case that: CustomHashInt =>
      (this eq that) || hash == that.hash && value == that.value
    case _ => false
  }

  override def toString: String = s"$value, [hash = $hash]"

}

object CustomHashInt {

  def apply(value: Int, hash: Int) = new CustomHashInt(value, hash)

}
