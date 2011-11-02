object Test {

  def main(args: Array[String]) {
    XmlEx.run()
    XmlEy.run()
    XmlPat.run()
    DodgyNamespace.run()
  }

  import scala.xml.{NodeSeq, Utility}
  import NodeSeq.seqToNodeSeq

  val ax = <hello foo="bar" x:foo="baz" xmlns:x="the namespace from outer space">
             <world/>
           </hello>

  val cx = <z:hello foo="bar" xmlns:z="z" x:foo="baz" xmlns:x="the namespace from outer space">
             crazy text world
           </z:hello>

  val bx = <hello foo="bar&amp;x"></hello>

  object XmlEx {

    def run() {
      assert((ax \ "@foo") xml_== "bar")              // uses NodeSeq.view!
      assert((ax \ "@foo") xml_== xml.Text("bar"))    // dto.
      assert((bx \ "@foo") xml_== "bar&x")            // dto.
      assert((bx \ "@foo") xml_sameElements List(xml.Text("bar&x")))
      assert("<hello foo=\"bar&amp;x\"></hello>" == bx.toString)
    }
  }

  object XmlEy {
    def run() {
      val z = ax \ "@{the namespace from outer space}foo"
      assert((ax \ "@{the namespace from outer space}foo") xml_== "baz")
      assert((cx \ "@{the namespace from outer space}foo") xml_== "baz")
 
      try {
        ax \ "@"
        assert(false)
      } catch {
        case _: IllegalArgumentException => 
      }
      try {
        ax \ "@{"
        assert(false)
      } catch {
        case _: IllegalArgumentException => 
      }
      try {
        ax \ "@{}"
        assert(false)
      } catch {
        case _: IllegalArgumentException => 
      }
 
    }
  }

  object XmlPat {
    def run() {
      assert(<hello/> match { case <hello/> => true; case _ => false; })
      assert(<x:ga xmlns:x="z"/> match { case <x:ga/> => true; case _ => false; });
      assert(Utility.trim(cx) match { case n @ <hello>crazy text world</hello> if (n \ "@foo") xml_== "bar" => true; })
      assert(Utility.trim(cx) match { case n @ <z:hello>crazy text world</z:hello> if (n \ "@foo") xml_== "bar" => true; })
    }
  }

  object DodgyNamespace {
    def run() {
      val x = <flog xmlns:ee="http://ee.com"><foo xmlns:dog="http://dog.com"><dog:cat/></foo></flog>
      assert(x.toString.matches(".*xmlns:dog=\"http://dog.com\".*"));
    }
  }

}
