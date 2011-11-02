package t743;
trait ParserXXX {
  val foo = null; 
  trait NodeImpl { 
    trait Link extends ParserXXX.this.Link {
      val from = null; 
    }
  }
  trait Link {
    val to0 = null;
  }
  trait IsLinked extends NodeImpl {
    trait Link extends super.Link;
  }
}
