import xml.{ NodeSeq, Null, Text, UnprefixedAttribute }

object Test {

  def main(args: Array[String]) {
    UnprefixedAttributeTest()
    AttributeWithOptionTest()
    AttributeOutputTest()
  }
  
  object UnprefixedAttributeTest {
    def apply() {
      val x = new UnprefixedAttribute("foo","bar", Null)
      println(Some(Text("bar")) == x.get("foo"))
      println(Text("bar") == x("foo"))
      println(None == x.get("no_foo"))
      println(null == x("no_foo"))
      
      val y = x.remove("foo")
      println(Null == y)

      val z = new UnprefixedAttribute("foo", null:NodeSeq, x)
      println(None == z.get("foo"))
      
      var appended = x append x append x append x
      var len = 0; while (appended ne Null) {
        appended = appended.next
        len = len + 1
      }
      println("removal of duplicates for unprefixed attributes in append = " + len)
    }
  }

  object AttributeWithOptionTest {
    def apply() {
      val x = new UnprefixedAttribute("foo", Some(Text("bar")), Null)

      println(Some(Text("bar")) == x.get("foo"))
      println(Text("bar") == x("foo"))
      println(None == x.get("no_foo"))
      println(null == x("no_foo"))

      val attr1 = Some(Text("foo value"))
      val attr2 = None
      val y = <b foo={attr1} bar={attr2} />
      println(Some(Text("foo value")) == y.attributes.get("foo"));
      println(Text("foo value") == y.attributes("foo"))
      println(None == y.attributes.get("bar"))
      println(null == y.attributes("bar"))

      val z = new UnprefixedAttribute("foo", None, x)
      println(None == z.get("foo"))
    }
  }

  object AttributeOutputTest {
    def apply() {
      println(<b x="&amp;"/>)
      println(<b x={"&"}/>)
    }
  }

}
