

import collection.immutable._


// ticket #3508
object Test {
  def main(args: Array[String]) {
    assert(Stream.tabulate(123)(_ + 1).toList == List.tabulate(123)(_ + 1))
  }
}
