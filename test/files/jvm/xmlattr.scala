import testing.SUnit.{Assert, TestCase, TestConsoleMain, TestSuite}
import xml.{NodeSeq, Null, Text, UnprefixedAttribute}

object Test extends TestConsoleMain {
  def suite = new TestSuite(UnprefixedAttributeTest, AttributeWithOptionTest)
  
  object UnprefixedAttributeTest extends TestCase("UnprefixedAttribute") with Assert {
    override def runTest {
      var x = new UnprefixedAttribute("foo","bar", Null)

      // always assertX(expected, actual)
      assertEquals(Some(Text("bar")), x.get("foo"));
      assertEquals(Text("bar"), x("foo"))
      assertEquals(None, x.get("no_foo"))
      assertEquals(null, x("no_foo"))
      
      val y = x.remove("foo")
      assertEquals(Null, y)

      val z = new UnprefixedAttribute("foo", null:NodeSeq, x)
      assertEquals(None, z.get("foo"))
      
      var appended = x append x append x append x
      var len = 0; while (appended ne Null) {
        appended = appended.next
        len = len + 1
      }
      assertEquals("removal of duplicates for unprefixed attributes in append", 1, len)
    }
  }

  object AttributeWithOptionTest extends TestCase("AttributeWithOption") with Assert {
    override def runTest {
      var x = new UnprefixedAttribute("foo", Some(Text("bar")), Null)

      assertEquals(Some(Text("bar")), x.get("foo"));
      assertEquals(Text("bar"), x("foo"))
      assertEquals(None, x.get("no_foo"))
      assertEquals(null, x("no_foo"))

      val attr1 = Some(Text("foo value"))
      val attr2 = None
      val y = <b foo={attr1} bar={attr2} />
      assertEquals(Some(Text("foo value")), y.attributes.get("foo"));
      assertEquals(Text("foo value"), y.attributes("foo"))
      assertEquals(None, y.attributes.get("bar"))
      assertEquals(null, y.attributes("bar"))

      val z = new UnprefixedAttribute("foo", None, x)
      assertEquals(None, z.get("foo")) // None
    }
  }

  object AttributeOutputTest extends TestCase("AttributeOutput") with Assert {
    override def runTest {
      assertEquals(<b x="&amp;"/>.toString, "<b x=\"&amp;\"></b>")
      assertEquals( <b x={"&"}/>.toString,  "<b x=\"&amp;\"></b>")
    }
  }
}
