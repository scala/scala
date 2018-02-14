

import collection.immutable._



object Test {
  def main(args: Array[String]): Unit = {
    val treemap = TreeMap(1 -> 2, 3 -> 4) ++ TreeMap(5 -> 6)
    (treemap: TreeMap[Int, Int])
  }
}
