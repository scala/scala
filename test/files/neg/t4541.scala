





@SerialVersionUID(1L)
final class Sparse[@specialized(Int) T](d: Array[T]) extends Serializable {
  protected var data: Array[T] = d
  def set(that: Sparse[T]) = {
    that.data
  }
}



