import scala.collection.generic.CanBuildFrom
import scala.collection._

object Test {
  def collect[A, Res](r: Traversable[A])(implicit bf: generic.CanBuild[A, Res]) = {
    val b: collection.mutable.Builder[A, Res] = bf()
    r foreach ((a: A) => b += a)
    b.result
  }

  collect[Int, Vector[Int]](List(1,2,3,4))
  collect[Char, String](List('1','2','3','4'))
  collect[Char, Array[Char]](List('1','2','3','4'))
}