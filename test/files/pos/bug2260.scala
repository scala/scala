package top

class Text(val value: String) extends Ordered[Text] {
  def compare(that: Text) = value.compare(that.value)
}

object Index {
  import scala.collection.immutable.TreeMap
  val tree = TreeMap.empty[Text, String]
}
