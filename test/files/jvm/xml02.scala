import testing.SUnit._

object Test extends TestConsoleMain {

  import scala.xml.{NodeSeq, Utility}
  import NodeSeq.view

  val ax = <hello foo="bar">
             <world/>
           </hello>

  val cx = <z:hello foo="bar" xmlns:z="z">
             crazy text world
           </z:hello>

  val bx = <hello foo="bar&amp;x"></hello>

  class XmlEx extends TestCase("attributes") with Assert {

    override def runTest = {
      assertTrue("@one",       ax \ "@foo" == "bar")              // uses NodeSeq.view!
      assertTrue("@two",       ax \ "@foo" == xml.Text("bar"))    // dto.
      assertTrue("@three",     bx \ "@foo" == "bar&x")            // dto.
      assertTrue  ("@four", (bx \ "@foo") sameElements List(xml.Text("bar&x")))
      //assertTrue("@four", (bx \ "@foo") sameElements List(xml.Text("bar"),xml.EntityRef("amp"),xml.Text("x")))
      assertEquals("@five",  "<hello foo=\"bar&amp;x\"></hello>", bx.toString)
    }
  }

  class XmlPat extends TestCase("patterns") with Assert {
    override def runTest = {
      assertTrue(<hello/> match { case <hello/> => true; case _ => false; })
      assertTrue(<x:ga xmlns:x="z"/> match { case <x:ga/> => true; case _ => false; });
      assertTrue(Utility.trim(cx) match { case n @ <hello>crazy text world</hello> if n \ "@foo" == "bar" => true; })
      assertTrue(Utility.trim(cx) match { case n @ <z:hello>crazy text world</z:hello> if n \ "@foo" == "bar" => true; })
    }
  }

  def suite = new TestSuite(
    new XmlEx,
	new XmlPat
  )
}
