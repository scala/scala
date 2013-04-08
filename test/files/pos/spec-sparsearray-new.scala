import scala.reflect.{ClassTag, classTag}
import scala.collection.mutable.MapLike

class SparseArray[@specialized(Int) T:ClassTag] extends collection.mutable.Map[Int,T] with collection.mutable.MapLike[Int,T,SparseArray[T]] {
  override def get(x: Int) = {
    val ind = findOffset(x)
    if(ind < 0) None else Some(sys.error("ignore"))
  }

  /**
   * Returns the offset into index and data for the requested vector
   * index.  If the requested index is not found, the return value is
   * negative and can be converted into an insertion point with -(rv+1).
   */
  private def findOffset(i : Int) : Int = {
    sys.error("impl doesn't matter")
  }

  override def apply(i : Int) : T = { sys.error("ignore") }
  override def update(i : Int, value : T) = sys.error("ignore")
  override def empty = new SparseArray[T]
  def -=(ind: Int) = sys.error("ignore")
  def +=(kv: (Int,T)) = sys.error("ignore")
  override final def iterator = sys.error("ignore")
}
