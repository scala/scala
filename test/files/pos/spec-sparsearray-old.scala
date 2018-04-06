import scala.reflect.ClassManifest
import scala.collection.{MapFactory, mutable}
class SparseArray[@specialized(Int) T:ClassManifest] extends collection.mutable.Map[Int,T] with collection.mutable.MapOps[Int,T,collection.mutable.Map,SparseArray[T]] {
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

  def addOne(elem: (Int, T)): SparseArray.this.type = ???
  def clear(): Unit = ???
  def iterator(): Iterator[(Int, T)] = ???
  def subtractOne(elem: Int): SparseArray.this.type = ???

  override protected[this] def fromSpecificIterable(coll: Iterable[(Int, T)]): SparseArray[T] = ???
  override protected[this] def newSpecificBuilder(): mutable.Builder[(Int, T), SparseArray[T]] = ???
  override def empty: SparseArray[T] = ???
}
