// regression test from LazyListTest
//
class L { val ll: LazyList[Nothing] = LazyList.empty #::: ll }

class M {
  def arg: LazyList[Int] = LazyList.empty[Int]
  def ll: LazyList[Int] = arg #::: ll
}

/* was
  private[this] val ll: scala.collection.immutable.LazyList[Nothing] = {
    final <synthetic> <artifact> val rassoc$1: scala.collection.immutable.LazyList[A] = LazyList.empty;
    immutable.this.LazyList.toDeferrer[Nothing](L.this.ll).#:::[Nothing](rassoc$1[Nothing])
  };
 */
