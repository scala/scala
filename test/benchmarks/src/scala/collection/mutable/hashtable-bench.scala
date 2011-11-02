import scala.collection.mutable.HashMap

object Test {
  var dummy: Long = 0L
  var _foreach: Long  = 0L
  var _iterator: Long = 0L

  def numbers: Seq[Int] = 1 to 1000000
  val map: HashMap[Int, Int] = HashMap(numbers zip numbers: _*)

  @inline final def timed(body: => Unit): Long = {
    val start = System.nanoTime
    body
    System.nanoTime - start
  }

  def go(xs: Iterable[Int], reps: Int) = {
    _foreach = 0L
    _iterator = 0L    

    0 until reps foreach { _ =>
      _foreach  += timed(xs foreach (dummy += _))
      _iterator += timed(xs.iterator foreach (dummy += _))
    }
    
    "  foreach avg " + (_foreach / reps) + "\n  iterator avg " + (_iterator / reps) + "\n"
  }
  
  def go2(xs: collection.Map[Int, Int], reps: Int) = {
    _foreach = 0L
    _iterator = 0L    
    
    def incDummy(nums: (Int, Int)) = {
      dummy += nums._1
      dummy -= nums._2
    }

    0 until reps foreach { _ =>
      _foreach  += timed(xs foreach incDummy)
      _iterator += timed(xs.iterator foreach incDummy)
    }

    "  foreach avg " + (_foreach / reps) + "\n  iterator avg " + (_iterator / reps) + "\n"
  }

  def main(args: Array[String]): Unit = {
    println("map.keys:")
    go(map.keys, 10) // warm
    println(go(map.keys, 10))
    
    println("map.values:")
    go(map.values, 10) // warm
    println(go(map.values, 10))
  
    println("map:")
    go2(map, 10) // warm
    println(go2(map, 10))
    
    println("// pay me no mind ... " + dummy)
  }
}
