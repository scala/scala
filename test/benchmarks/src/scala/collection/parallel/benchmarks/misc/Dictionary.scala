package scala.collection.parallel.benchmarks.misc




object Dictionary {
  val words = wordlines.split(System.getProperty("line.separator")).filter(_.trim != "").toList
  def wordlines = {
    val is = getClass.getClassLoader.getResourceAsStream("scala/collection/parallel/benchmarks/misc/dict.txt")
    scala.io.Source.fromInputStream(is).mkString
  }
}
