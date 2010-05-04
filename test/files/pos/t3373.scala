class Entry(time: Long) {
 def getTime: Long = time
}

object Test {
  def extractTime(e: Entry) = e.getTime

  implicit val orderEntries = new Ordering[Entry] {
   def compare(first: Entry, second: Entry) = extractTime(first) compare extractTime(second)
  }
}