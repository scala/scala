


import scala.collection.JavaConverters._



object Test extends App {
  
  def bench(label: String)(body: => Unit): Long = {
    val start = System.nanoTime

    0.until(10).foreach(_ => body)

    val end = System.nanoTime

    //println("%s: %s ms".format(label, (end - start) / 1000.0 / 1000.0))
    
    end - start
  }
  
  def benchJava(values: java.util.Map[Int, Int]) = {
    bench("Java Map") {
      val m = new java.util.HashMap[Int, Int]
      
      m.putAll(values)
    }
  }

  def benchScala(values: Iterable[(Int, Int)]) = {
    bench("Scala Map") {
      val m = new scala.collection.mutable.HashMap[Int, Int]
      
      m ++= values
    }
  }
  
  def benchScalaSorted(values: Iterable[(Int, Int)]) = {
    bench("Scala Map sorted") {
      val m = new scala.collection.mutable.HashMap[Int, Int]
      
      m ++= values.toArray.sorted
    }
  }
  
  def benchScalaPar(values: Iterable[(Int, Int)]) = {
    bench("Scala ParMap") {
      val m = new scala.collection.parallel.mutable.ParHashMap[Int, Int] map { x => x }
      
      m ++= values
    }
  }
  
  val total = 50000
  val values = (0 until total) zip (0 until total)
  val map = scala.collection.mutable.HashMap.empty[Int, Int]
  
  map ++= values
  
  // warmup
  for (x <- 0 until 5) {
    benchJava(map.asJava)
    benchScala(map)
    benchScalaPar(map)
    benchJava(map.asJava)
    benchScala(map)
    benchScalaPar(map)
  }
  
  val javamap = benchJava(map.asJava)
  val scalamap = benchScala(map)
  val scalaparmap = benchScalaPar(map)
  
  // println(javamap)
  // println(scalamap)
  // println(scalaparmap)
  
  assert(scalamap < (javamap * 4))
  assert(scalaparmap < (javamap * 4))
}








