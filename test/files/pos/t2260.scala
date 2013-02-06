package top

class Text(val value: String) extends Ordered[Text] {
  def compareTo(that: Text) = value.compareTo(that.value)
}

object Index {
  import scala.collection.immutable.TreeMap
  val tree = TreeMap.empty[Text, String]
}
