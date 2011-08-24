import collection.JavaConverters._
import java.{ util => ju }
import scala.collection.{ mutable, immutable }

object Test {
  def m[P <% AsJava[ju.List[Int]]](l: P) = 1
  m(List(1))
}