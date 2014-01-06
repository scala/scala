
import scala.collection.immutable.TreeMap
import scala.math.Ordering

class Test[K](param:TreeMap[K,Int]){
  def this() = this({
    implicit object TreeOrd extends Ordering[K](){
      def compare(a: K, b: K) = {
        -1
      }
    }
    new TreeMap[K, Int]()
  })
}

object Test extends App {
  new Test()
}
