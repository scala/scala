object Test {
  def main(args: Array[String]): Unit = {
    val size = 100 * 1024
    val doubled = (1 to size) ++ (1 to size)

    println("Array: " + Array.tabulate(size)(x => x).distinct.size)
    println("Vector: " + Vector.tabulate(size)(x => x).distinct.size)
    println("List: " + List.tabulate(size)(x => x).distinct.size)
    println("Stream: " + Stream.tabulate(size)(x => x).distinct.size)

    println("Array: " + doubled.toArray.distinct.size)
    println("Vector: " + doubled.toVector.distinct.size)
    println("List: " + doubled.toList.distinct.size)
    println("Stream: " + doubled.toStream.distinct.size)
  }
}
