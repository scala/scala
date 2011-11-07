import scala.collection.generic._
import scala.collection._

object Test {
  def collect[A, Res](r: Traversable[A])(implicit bf: CanBuild[A, Res]) = {
    val b = bf()
    for (a <- r) b += a
    b.result
  }
  
  collect[Int, Vector[Int]](List(1,2,3,4)) 
  collect[Char, String](List('1','2','3','4'))
  collect[Char, Array[Char]](List('1','2','3','4'))  
}