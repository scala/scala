import testing.SUnit._

object Test extends TestConsoleMain {

  import scala.xml.{NodeSeq, Utility}
  import NodeSeq.seqToNodeSeq

  val ax = <hello foo="bar" x:foo="baz" xmlns:x="the namespace from outer space">
             <world/>
           </hello>

  val cx = <z:hello foo="bar" xmlns:z="z" x:foo="baz" xmlns:x="the namespace from outer space">
             crazy text world
           </z:hello>

  val bx = <hello foo="bar&amp;x"></hello>

  object XmlEx extends TestCase("attributes") with Assert {

    override def runTest = {
      assertTrue("@one",       (ax \ "@foo") xml_== "bar")              // uses NodeSeq.view!
      assertTrue("@two",       (ax \ "@foo") xml_== xml.Text("bar"))    // dto.
      assertTrue("@three",     (bx \ "@foo") xml_== "bar&x")            // dto.
      assertTrue  ("@four", (bx \ "@foo") xml_sameElements List(xml.Text("bar&x")))
      assertEquals("@five",  "<hello foo=\"bar&amp;x\"></hello>", bx.toString)
    }
  }

  object XmlEy extends TestCase("attributes with namespace") with Assert {
    override def runTest = {
      val z = ax \ "@{the namespace from outer space}foo"
      assertTrue("@six",   (ax \ "@{the namespace from outer space}foo") xml_== "baz")
      assertTrue("@eight", (cx \ "@{the namespace from outer space}foo") xml_== "baz")
 
      try {
        ax \ "@"
        assertTrue("wrong1", false)
      } catch {
        case _: IllegalArgumentException => 
      }
      try {
        ax \ "@{"
        assertTrue("wrong2", false)
      } catch {
        case _: IllegalArgumentException => 
      }
      try {
        ax \ "@{}"
        assertTrue("wrong3", false)
      } catch {
        case _: IllegalArgumentException => 
      }
 
    }
  }

  object XmlPat extends TestCase("patterns") with Assert {
    override def runTest = {
      assertTrue(<hello/> match { case <hello/> => true; case _ => false; })
      assertTrue(<x:ga xmlns:x="z"/> match { case <x:ga/> => true; case _ => false; });
      assertTrue(Utility.trim(cx) match { case n @ <hello>crazy text world</hello> if (n \ "@foo") xml_== "bar" => true; })
      assertTrue(Utility.trim(cx) match { case n @ <z:hello>crazy text world</z:hello> if (n \ "@foo") xml_== "bar" => true; })
    }
  }

  object DodgyNamespace extends TestCase("DodgyNamespace") with Assert {
    override def runTest = {
      val x = <flog xmlns:ee="http://ee.com"><foo xmlns:dog="http://dog.com"><dog:cat/></foo></flog>
      assertTrue(x.toString.matches(".*xmlns:dog=\"http://dog.com\".*"));
    }
  }
  def suite = new TestSuite(
    XmlEx,
    XmlEy,
    XmlPat,
    DodgyNamespace
  )
}
