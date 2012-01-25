


import collection.parallel.CompositeThrowable



object Test {
  
  def main(args: Array[String]) {
    val foos = (1 to 1000) toSeq;
    try {
      foos.par.map(i => if (i % 37 == 0) sys.error("i div 37") else i)
    } catch {
      case CompositeThrowable(thr) => println("Composite throwable")
    }
  }
  
}
