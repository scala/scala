package scala.collection.mutable
import org.scalacheck._
import org.scalacheck.Prop._
import scala.collection.immutable
object LinkedHashMapProperties extends Properties("mutable.LinkedHashMap") {
  override def overrideParameters(p: Test.Parameters): Test.Parameters =
    p.withInitialSeed(42L)

  property("filterInPlace(p)") = forAll { v: Vector[(Int, Int)] =>
    val p = (i: Int, j: Int) => (i + j) % 2 == 0
    val expected: collection.Map[Int, Int] = v.to(immutable.HashMap).filter(p.tupled)
    val actual: collection.Map[Int, Int] = v.to(LinkedHashMap).filterInPlace(p)
    actual ?= expected
  }
}