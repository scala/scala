object Test {
  
  def f(s: String) = {
    <entry>
    {
      for (item <- s split ',') yield
        <elem>{ item }</elem>
    }
    </entry>
  }
  
  def main(args: Array[String]): Unit = {
    println(f("a,b,c"))
  }
}
