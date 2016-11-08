package mixins;

import scala.collection.mutable._;

class Collections extends HashSet[Int] {
  override def +=(elem: Int): this.type = super.+=(elem)
  override def -=(elem: Int): this.type = super.-=(elem)
  override def clear: Unit = super.clear
}

object collections extends Collections
