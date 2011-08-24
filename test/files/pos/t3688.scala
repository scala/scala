import collection.mutable
import collection.JavaConversions._
import java.{util => ju}

object Test {

 implicitly[mutable.Map[Int, String] => ju.Dictionary[Int, String]]

}

object Test2 {
  def m[P <% ju.List[Int]](l: P) = 1
  m(List(1)) // bug: should compile
}