
object Test {

  import scala.testing.SUnit._
  import scala.xml.{MetaData, Null, Parsing, PrefixedAttribute, UnprefixedAttribute }

  class ParsingTest extends TestCase("scala.xml.Parsing") with Assert {
    override def runTest = {
      assertTrue(Parsing.isNameStart('b'))
    }
  }
  class MetaDataTest extends TestCase("scala.xml.MetaData") with Assert {

    import scala.xml.{TopScope, NamespaceBinding, Atom, Text }

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

    }
  }

  class UtilityTest extends TestCase("scala.xml.Utility") with Assert {
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
  }

  def main(args:Array[String]) = {
    val ts = new TestSuite(
      new ParsingTest,
      new MetaDataTest,
      new UtilityTest
    )
    val tr = new TestResult()
    ts.run(tr)
    for(val failure <- tr.failures) {
      Console.println(failure)
    }
  }
}
