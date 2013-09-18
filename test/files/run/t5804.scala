

import collection.mutable._


object Test {

  def main(args: Array[String]) {
    class CustomHashMap extends HashMap[Int, Int] {
      override def initialSize = 65

      println(table.length)
    }

    new CustomHashMap
    new HashMap {
      println(table.length)
    }

    class CustomHashSet extends HashSet[Int] {
      override def initialSize = 96

      println(table.length)
    }

    new CustomHashSet
    new HashSet {
      println(table.length)
    }
  }

}
