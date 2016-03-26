import scala.collection.mutable.WeakHashMap
import scala.collection.JavaConverters._

class bar { }

class foo {
  val map = WeakHashMap[AnyRef, collection.mutable.Map[bar, collection.mutable.Set[bar]]]()

  def test={
    val tmp:bar=null
    if (map.get(tmp).isEmpty) map.put(tmp,collection.mutable.Set())
  }
}
