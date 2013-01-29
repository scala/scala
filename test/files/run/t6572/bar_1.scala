package bar

abstract class IntBase[V] extends Base[Int, V]

class DefaultIntBase[V <: IntProvider] extends IntBase[V] {
  override protected def hashCode(key: Int) = key
}

trait IntProvider {
  def int: Int
}

abstract class Base[@specialized K, V] {

  protected def hashCode(key: K) = key.hashCode

  def get(key: K): V = throw new RuntimeException

}