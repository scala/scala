
object Test {

  import scala.testing.SUnit._
  import scala.xml.{MetaData, Null, Utility, PrefixedAttribute, UnprefixedAttribute }

  class MetaDataTest extends TestCase("scala.xml.MetaData") with Assert {

    import scala.xml.{HasKeyValue, TopScope, NamespaceBinding, Node, Atom, Text }

	def domatch(x:Node): Node = {
      val hasBar = new HasKeyValue("bar")
	  x match {
		//case Node("foo", hasBar(z), _*) => z
            case Node("foo", md, _*) if !hasBar.unapplySeq(md).isEmpty => 
                 md("bar")(0)
			case _ => new Atom(3)
	  }
	}
    override def runTest = {

      var x: MetaData         = Null
      var s: NamespaceBinding = TopScope

      // testing method def apply(uri:String, scp:NamespaceBinding, k:String): Seq[Node] 
      //                def apply(k:String): Seq[Node] 

      assertEquals("absent element (prefixed) 1",  null, x("za://foo.com", s, "bar" ))
      assertEquals("absent element (unprefix) 1",  null, x("bar"))

      assertEquals("absent element (prefixed) 2",  None, x.get("za://foo.com", s, "bar" ))
      assertEquals("absent element (unprefix) 2",  None, x.get("bar"))

      x = new PrefixedAttribute("zo","bar", new Atom(42), x)
      s = new NamespaceBinding("zo","za://foo.com",s)

      assertEquals("present element (prefixed) 3",  new Atom(42), x("za://foo.com", s, "bar" ))
      assertEquals("present element (unprefix) 3",  null, x("bar"))

      assertEquals("present element (prefixed) 4",  Some(new Atom(42)), x.get("za://foo.com", s, "bar" ))
      assertEquals("present element (unprefix) 4",  None, x.get("bar"))

      x = new UnprefixedAttribute("bar","meaning", x)

      assertEquals("present element (prefixed) 5",  null, x(null, s, "bar" ))
      assertEquals("present element (unprefix) 5",  Text("meaning"), x("bar"))

      assertEquals("present element (prefixed) 6",  None, x.get(null, s, "bar" ))
      assertEquals("present element (unprefix) 6",  Some(Text("meaning")), x.get("bar"))

	  val z =  <foo bar="gar"/>
	  val z2 = <foo/>

	 assertEquals("attribute extractor 1", Text("gar"), domatch(z)) 
	 assertEquals("attribute extractor 2", new Atom(3), domatch(z2)) 

    }
  }

  class UtilityTest extends TestCase("scala.xml.Utility") with Assert {
    def runTest() = {
      assertTrue(Utility.isNameStart('b'))
      assertFalse(Utility.isNameStart(':'))
      
      
    val x = <foo>
               <toomuchws/>
            </foo>

    val y = xml.Utility.trim(x)
    
    assertEquals("trim 1 ", 1, y match { case <foo><toomuchws/></foo> => 1 })

    val x2 = <foo>
               <toomuchws>  a b  b a  </toomuchws>
            </foo>

    val y2 = xml.Utility.trim(x2)

    assertEquals("trim 2 ", 2, y2 match { case <foo><toomuchws>a b b a</toomuchws></foo> => 2 })


    val z = <bar>''</bar>
    val z1 = z.toString

    assertEquals("apos unescaped", "<bar>''</bar>", z1)

	val q = xml.Utility.sort(<a g='3' j='2' oo='2' a='2'/>)
	assertEquals("sort attrib"+xml.Utility.sort(q.attributes).toString, " a=\"2\" g=\"3\" j=\"2\" oo=\"2\"", xml.Utility.sort(q.attributes).toString)
	 val pp = new xml.PrettyPrinter(80,5)
	assertEquals("pretty print sorted attrib:"+pp.format(q), "<a a=\"2\" g=\"3\" j=\"2\" oo=\"2\"></a>", pp.format(q))

    <hi>
      <there/>
      <guys/>
    </hi>.hashCode // Bug #777
  }
  }

  def main(args:Array[String]) = {
    val ts = new TestSuite(
      new MetaDataTest,
      new UtilityTest 
    )
    val tr = new TestResult()
    ts.run(tr)
    tr.failures foreach Console.println
  }
}
