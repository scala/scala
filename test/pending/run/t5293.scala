


import scala.collection.JavaConverters._



object Test extends App {
  
  def bench(label: String)(body: => Unit): Long = {
    val start = System.nanoTime

    0.until(10).foreach(_ => body)

    val end = System.nanoTime

    //println("%s: %s ms".format(label, (end - start) / 1000.0 / 1000.0))
    
    end - start
  }
  
  def benchJava(values: java.util.Collection[Int]) = {
    bench("Java Set") {
      val set = new java.util.HashSet[Int]
      
      set.addAll(values)
    }
  }

  def benchScala(values: Iterable[Int]) = {
    bench("Scala Set") {
      val set = new scala.collection.mutable.HashSet[Int]
      
      set ++= values
    }
  }
  
  def benchScalaSorted(values: Iterable[Int]) = {
    bench("Scala Set sorted") {
      val set = new scala.collection.mutable.HashSet[Int]
      
      set ++= values.toArray.sorted
    }
  }
  
  def benchScalaPar(values: Iterable[Int]) = {
    bench("Scala ParSet") {
      val set = new scala.collection.parallel.mutable.ParHashSet[Int] map { x => x }
      
      set ++= values
    }
  }
  
  val values = 0 until 50000
  val set = scala.collection.mutable.HashSet.empty[Int]
  
  set ++= values
  
  // warmup
  for (x <- 0 until 5) {
    benchJava(set.asJava)
    benchScala(set)
    benchScalaPar(set)
    benchJava(set.asJava)
    benchScala(set)
    benchScalaPar(set)
  }
  
  val javaset = benchJava(set.asJava)
  val scalaset = benchScala(set)
  val scalaparset = benchScalaPar(set)
  
  assert(scalaset < (javaset * 8), "scalaset: " + scalaset + " vs. javaset: " + javaset)
  assert(scalaparset < (javaset * 8), "scalaparset: " + scalaparset + " vs. javaset: " + javaset)
}








