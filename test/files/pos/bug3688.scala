import collection.JavaConversions._
import java.{ util => ju }

object Test {
  def m[P <% ju.List[Int]](l: P) = 1
  m(List(1)) // bug: should compile
}