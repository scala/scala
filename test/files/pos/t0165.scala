package test3
import scala.collection.mutable.LinkedHashMap

trait Main {
  def asMany : ArrayResult = {
    object result extends LinkedHashMap[String,String] with ArrayResult {
      def current = result
    }
    result
  }
  trait ArrayResult {
    def current : scala.collection.Map[String,String]
  }
}
