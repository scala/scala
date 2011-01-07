package scala.collection.parallel.benchmarks.misc




object Dictionary {
  val wordlist = wordlines.split(System.getProperty("line.separator")).filter(_.trim != "").toList
  val wordarray = wordlist.toArray
  def wordlines = {
    val is = getClass.getClassLoader.getResourceAsStream("scala/collection/parallel/benchmarks/misc/dict.txt")
    scala.io.Source.fromInputStream(is).mkString
  }
}
