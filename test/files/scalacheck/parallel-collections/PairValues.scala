package scala.collection.parallel.ops





import org.scalacheck._
import org.scalacheck.Gen
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck.Properties
import org.scalacheck.Arbitrary._




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
