import testing.SUnit._

object Test extends TestConsoleMain {

  import scala.xml.NodeSeq
  import NodeSeq.view

  val ax = <hello foo="bar">
             <world/>
           </hello>

  val bx = <hello foo="bar&amp;x"></hello>

  class XmlEx extends TestCase("attributes") with Assert {

    override def run = {
      assertEquals("@one",   "bar",           ax \ "@foo")
      assertEquals("@two",   xml.Text("bar"), ax \ "@foo")
      assertEquals("@three", "bar&x",         bx \ "@foo")
      assertTrue  ("@four", (bx \ "@foo") sameElements List(xml.Text("bar&x")))
      //assertTrue("@four", (bx \ "@foo") sameElements List(xml.Text("bar"),xml.EntityRef("amp"),xml.Text("x")))
      assertEquals("@five",  "<hello foo=\"bar&amp;x\"></hello>", bx.toString)
    }
  }

  class XmlPat extends TestCase("patterns") with Assert {
    override def run = {
      assertTrue(<hello/> match { case <hello/> => true; case _ => false; })
      assertTrue(<x:ga xmlns:x="z"/> match { case <x:ga/> => true; case _ => false; });

  /*
  assertEquals(ax match { case x @ <hello>
                               <world/>
                           </hello> if x \ "@foo" == "bar" => true;
                      case _ => false; },
               true);

  assertEquals(
     <hello foo="bar">
       crazy text world
     </hello> match { case <hello>
                               crazy   text  world
                           </hello> => true;
                      case _ => false; },
               true);
  */
  }
  }
  def suite = new TestSuite(
    new XmlEx,
	new XmlPat
  )
}
