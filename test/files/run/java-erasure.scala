object Test {
  val list = new java.util.ArrayList[String] { };
  list add "a"
  list add "c"
  list add "b"
  
  def main(args: Array[String]): Unit = {
    println(java.util.Collections.max(list))
  }
}
