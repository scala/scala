import scala.collection.mutable.MapLike

class SparseArray[@specialized(Int) T:ClassManifest] extends collection.mutable.Map[Int,T] with collection.mutable.MapLike[Int,T,SparseArray[T]] {
  override def get(x: Int) = {
    val ind = findOffset(x)
    if(ind < 0) None else Some(error("ignore"))
  }

  /**
   * Returns the offset into index and data for the requested vector
   * index.  If the requested index is not found, the return value is
   * negative and can be converted into an insertion point with -(rv+1).
   */
  private def findOffset(i : Int) : Int = {
    error("impl doesn't matter")
  }

  override def apply(i : Int) : T = { error("ignore") }
  override def update(i : Int, value : T) = error("ignore")
  override def empty = new SparseArray[T]
  def -=(ind: Int) = error("ignore")
  def +=(kv: (Int,T)) = error("ignore")
  override final def iterator = error("ignore")
}
