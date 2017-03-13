package benchmark

/** Provider of pair [[Set[T], Set[T]]
  * 
  * @tparam T the type of the keys
  */
trait TwoSetInRatioProvider[T] {
  /** Return new instance of T base on int index. */
  def instance(idx: Int): T

  /**
    * |---s1--|---s1-&-s2--|-------s2-------| ; |s1|+|s2| = size; |s1|/|s2| = ratio; |s1 & s2|/n = intersection
    * Return a pair of Sets having size, ratio and intersection. */
  def build(size: Int, ratio: Float, intersection: Float): (scala.collection.immutable.Set[T], scala.collection.immutable.Set[T]) = {
    val keys = (0 until size).map(instance).toList
    val s2Start = Math.round(size * (1 - intersection) * ratio / (1 + ratio))
    val s1End = Math.round(s2Start + size * intersection * ratio)

    val set1 = keys.take(s1End).toSet
    val set2 = keys.drop(s2Start).toSet

    (set1, set2)
  }
}

object TwoSetInRatioProvider {

  implicit object TwoSetInRatioIntProviÅder extends TwoSetInRatioProvider[Int] {
    override def instance(idx: Int): Int = idx
  }

}
