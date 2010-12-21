




object Dictionary {
  val wordlist = wordlines.split(System.getProperty("line.separator")).filter(_.trim != "").toList
  val wordarray = wordlist.toArray
  def wordlines = scala.io.Source.fromFile("test/files/run/coder/dict.txt").mkString
}
