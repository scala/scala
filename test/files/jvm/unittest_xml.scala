import scala.xml.{ MetaData, Null, Utility, PrefixedAttribute, UnprefixedAttribute }

object Test {

  def main(args:Array[String]) = {
    MetaDataTest.run()
    UtilityTest.run()
  }

  object MetaDataTest {

    import scala.xml.{ TopScope, NamespaceBinding, Node, Atom, Text }

    def domatch(x:Node): Node = {
      x match {
            case Node("foo", md @ UnprefixedAttribute(_, value, _), _*) if !value.isEmpty =>
                 md("bar")(0)
            case _ => new Atom(3)
      }
    }

    def run() {

      var x: MetaData         = Null
      var s: NamespaceBinding = TopScope

      // testing method def apply(uri:String, scp:NamespaceBinding, k:String): Seq[Node] 
      //                def apply(k:String): Seq[Node] 

      assert(null == x("za://foo.com", s, "bar" ), "absent element (prefixed) 1")
      assert(null == x("bar"), "absent element (unprefix) 1")

      assert(None == x.get("za://foo.com", s, "bar" ), "absent element (prefixed) 2")
      assert(None == x.get("bar"), "absent element (unprefix) 2")

      x = new PrefixedAttribute("zo","bar", new Atom(42), x)
      s = new NamespaceBinding("zo","za://foo.com",s)

      assert(new Atom(42) == x("za://foo.com", s, "bar" ), "present element (prefixed) 3")
      assert(null == x("bar"), "present element (unprefix) 3")

      assert(Some(new Atom(42)) == x.get("za://foo.com", s, "bar" ), "present element (prefixed) 4")
      assert(None == x.get("bar"), "present element (unprefix) 4")

      x = new UnprefixedAttribute("bar","meaning", x)

      assert(null == x(null, s, "bar"), "present element (prefixed) 5")
      assert(Text("meaning") == x("bar"), "present element (unprefix) 5")

      assert(None == x.get(null, s, "bar" ), "present element (prefixed) 6")
      assert(Some(Text("meaning")) == x.get("bar"), "present element (unprefix) 6")

      val z =  <foo bar="gar"/>
      val z2 = <foo/>

      assert(Text("gar") == domatch(z), "attribute extractor 1") 
      assert(new Atom(3) == domatch(z2), "attribute extractor 2") 

    }
  }

  object UtilityTest {
    def run() {
      assert(Utility.isNameStart('b'))
      assert(!Utility.isNameStart(':'))

      val x = <foo>
                 <toomuchws/>
              </foo>

      val y = xml.Utility.trim(x)

      assert(1 == (y match { case <foo><toomuchws/></foo> => 1 }), "trim 1")

      val x2 = <foo>
                 <toomuchws>  a b  b a  </toomuchws>
              </foo>

      val y2 = xml.Utility.trim(x2)

      assert(2 == (y2 match { case <foo><toomuchws>a b b a</toomuchws></foo> => 2 }), "trim 2")

      val z = <bar>''</bar>
      val z1 = z.toString

      assert("<bar>''</bar>" == z1, "apos unescaped")

      val q = xml.Utility.sort(<a g='3' j='2' oo='2' a='2'/>)
      assert(" a=\"2\" g=\"3\" j=\"2\" oo=\"2\"" == xml.Utility.sort(q.attributes).toString)

      val pp = new xml.PrettyPrinter(80,5)
      assert("<a a=\"2\" g=\"3\" j=\"2\" oo=\"2\"></a>" == pp.format(q))

      <hi>
        <there/>
        <guys/>
      </hi>.hashCode // Bug #777
    }
  }

}
