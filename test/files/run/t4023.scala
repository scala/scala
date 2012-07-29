object Test {
  object C {
    class B1
    private class B2
    object B3
    private object B4
    object B5 extends B1
    private object B6 extends B2 
 
    val valuesTry1 = this.getClass.getDeclaredClasses
    val valuesTry2 = C.getClass.getDeclaredClasses
    val valuesTry3 = getClass.getDeclaredClasses
  }
        
  def main(args: Array[String]) {
    println("Try 1: (" + C.valuesTry1.length + " classes)")
    C.valuesTry1.foreach(println)
    println("Try 2: (" + C.valuesTry2.length + " classes)")
    C.valuesTry2.foreach(println)
    println("Try 3: (" + C.valuesTry3.length + " classes)")
    C.valuesTry3.foreach(println)
  }
}