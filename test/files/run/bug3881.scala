object Test {

  def forever() = new io.Source {
    val iter = "<foo>".iterator ++ new Iterator[Char] {
      var count = -1
      val bar = "<bar/>\n"
      def hasNext = true
      def next() = {
        count += 1
        bar(count % bar.length)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val src = forever()
    val reader = new xml.pull.XMLEventReader(src)
    var count = 0
    while (reader.hasNext && count < 1000000) {
      reader.next
      count += 1
    }
    reader.stop
    println(count + " xml events")
  }
}

