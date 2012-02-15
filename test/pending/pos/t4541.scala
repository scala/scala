@SerialVersionUID(1L)
final class SparseArray[@specialized T](private var data : Array[T]) extends Serializable {
  def use(inData : Array[T]) = {
    data = inData;
  }
  
  def set(that : SparseArray[T]) = {
    use(that.data.clone)
  }
}