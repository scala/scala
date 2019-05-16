package scala.reflect.internal.names

abstract class AbstractNodeInterner[T >: Null <: NameBase](val createName: String => T) extends NameTable[T] {
  type N <: Node[T]

  def size: Int

  @inline protected final def improveHash(hash: Int) = hash ^ hash >>> 16
  protected final def hashLikeString(chars: Array[Char], offset: Int, length: Int): Int = {
    var h = 0
    var index = offset
    val max = offset + length
    while (index < max) {
      h = 31 * h + chars(index)
      index += 1
    }
    h
  }


  final def contains(key: String): Boolean = {
    getExistingImpl(key) ne null
  }

  final def get(key: String): Option[T] = {
    Option(getExistingImpl(key))
  }

  def getExistingImpl(key: String): T

}
