class A {
  def f1 = {
    val x = 5
  
"""
hi"""
  }
  def f2 = {
    val x = 5
  
    """hi"""
  }  
  def f3 = {
    val x = 5
  
    "\nhi"
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    val x = new A
    import x._
    List(f1, f2, f3) foreach println
  }
}
