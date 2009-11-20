package mixins;

import scala.collection.mutable._;

class Collections extends HashSet[Int] with ObservableSet[Int] {
  override def +=(elem: Int): this.type = super.+=(elem);
  override def -=(elem: Int): this.type = super.-=(elem);
  override def clear: Unit = super.clear;

}

object collections extends Collections;

//class Collections1 extends HashSet[Int] with ObservableSet[Int,Collections1];
//object collections1 extends Collections1;
