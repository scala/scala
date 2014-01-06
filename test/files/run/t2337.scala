
object Test {

  def compare(first: Any, second: Any): Any = {
      (first, second) match {
        case (k: Int, o: Int) => k compare o
        //why the next case matches (Float, Int) but does not match (Int, Float) ???
        case (k: Number, o: Number) => k.doubleValue() compare o.doubleValue()
        case _ => "BOGON"
        // throw new Exception("Unsupported compare " + first + "; " + second)
    }
  }

  def main(args: Array[String]): Unit = {
    println("Both Int", -1, compare(0, 1))
    println("Both Float", 1, compare(1.0, 0.0))
    println("Float then Int", 0, compare(10.0, 10))
    println("Int then Float", 0, compare(10, 10.0)) //this fails with an exception
  }
}

