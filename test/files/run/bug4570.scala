object Test extends Enumeration {
  val foo = Value
  def bar = withName("foo")
  
  def main(args: Array[String]): Unit = {
    values foreach println 
  }
}
