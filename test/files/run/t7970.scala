object Test {
  {
    val session: Object = null
    trait Outer{
      trait Inner{
        assert(session == null)
      }          
    }
    val o = new Outer{}
    new o.Inner { }
  }

  def main(args: Array[String]): Unit = {
  }
}
