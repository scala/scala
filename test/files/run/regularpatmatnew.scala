object Test {
  import scala.testing.SUnit._

  def main(args:Array[String]): Unit = {
    val tr = new TestResult
    new TestSuite(

      new Test01,
      new Test02,
      new Test03

    ).run(tr)

    for(val f <- tr.failures())
      Console println f
  }

  class Test01 extends TestCase("numero uno (all ignoring patterns on List)") {
    def doMatch(l:List[String]):String = l match {
        case List(_*) => "ok"
    }
    override def runTest() = {
      val list1 = List();
      assertEquals(doMatch(list1), "ok");
      val list2 = List("1","2","3");
      assertEquals(doMatch(list2), "ok");
    }
  }

  /* these are not allowed, for strange reasons I will never figure out

    def doMatch(l:Seq[String]):String = l match {
        case List(_*) => "ok"
        case _ => "not ok"
    }

    def doMatch(l:Seq[String]):String = l match {
        case Array(_*) => "ok"
        case _ => "not ok"
    }
    */

  class Test02 extends TestCase("numero due (all ignoring patterns on Seq)") {
    def doMatch(l:Seq[String]):String = l match {
        case Seq(_*) => "ok"
    }
    override def runTest() = {
      val list1 = List();
      assertEquals(doMatch(list1), "ok");
      val list2 = List("1","2","3");
      assertEquals(doMatch(list2), "ok");
      val array3 = Array[String]();
      assertEquals(doMatch(array3), "ok");
      val array4 = Array[String]("ga","gu");
      assertEquals(doMatch(array4), "ok");
    }
  }

  class Test03 extends TestCase("numero tre (right-ignoring patterns on List, defaults)") {
    def doMatch(l:List[String]):String = l match {
        case List(_,_,_,_*) => "ok"
        case _ => "not ok"
    }
    override def runTest() = {
      val list1 = List();
      assertEquals(doMatch(list1), "not ok");
      val list2 = List("1","2","3");
      assertEquals(doMatch(list2), "ok");
      val list3 = List("1","2","3","4");
      assertEquals(doMatch(list3), "ok");
    }
  }

}
