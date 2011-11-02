// bug 1183 from in the old tracker, not in Trac

object Test {

  class Foo(j:Int) {
    object Baz
    class Bam
    object Bar
    case class Bar(i:Int)
  }

  
  class Test717 {
    val foo1 = new Foo(1)

    def runTest() = {
      val res = (foo1.Bar(2):Any) match {
        case foo1.Bar(2) => true   // (1)
      }
      require(res)
    }
  }

  // (2)
  object Foo {
    class Bar(val x : String)
    class Baz
    object Bam
    object Bar
    
    def unapply(s : String) : Option[Bar] = Some(new Bar(s))
  }

}
