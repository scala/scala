package scala.collection.parallel.ops





import org.scalacheck._
import org.scalacheck.Gen




trait PairValues[K, V] {
  def kvalues: Seq[Gen[K]]
  def vvalues: Seq[Gen[V]]

  def values = for {
    kg <- kvalues
    vg <- vvalues
  } yield for {
    k <- kg
    v <- vg
  } yield (k, v)
}
