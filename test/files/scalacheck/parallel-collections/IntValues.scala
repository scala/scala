package scala.collection.parallel.ops





import org.scalacheck._
import org.scalacheck.Gen
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck.Properties
import org.scalacheck.Arbitrary._




trait IntValues {
  def values = Seq(
      arbitrary[Int],
      arbitrary[Int] suchThat (_ >= 0),
      arbitrary[Int] suchThat (_ < 0),
      choose(0, 0),
      choose(0, 10),
      choose(0, 100)
      )
}
