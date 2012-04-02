/** It's a pos test because it does indeed compile,
 *  not so much because I'm glad it does.  Testing
 *  that error messages created and discarded during
 *  implicit search don't blow it up.
 */

import scala.collection.mutable.WeakHashMap
import scala.collection.JavaConversions._

class bar { }

class foo {
  val map = WeakHashMap[AnyRef, collection.mutable.Map[bar, collection.mutable.Set[bar]]]()

  def test={
    val tmp:bar=null
    if (map.get(tmp).isEmpty) map.put(tmp,collection.mutable.Set())
  }
}
