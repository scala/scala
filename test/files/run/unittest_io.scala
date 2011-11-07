import testing.SUnit._

object Test extends TestConsoleMain {
  def suite = new TestSuite(new UTF8Tests, new SourceTest)

  class UTF8Tests extends TestCase("UTF8Codec") {
    import io.UTF8Codec.encode
   
    def runTest {
      assertEquals(new String( encode(0x004D), "utf8"), new String(Array(0x004D.asInstanceOf[Char])))
      assertEquals(new String( encode(0x0430), "utf8"), new String(Array(0x0430.asInstanceOf[Char])))
      assertEquals(new String( encode(0x4E8C), "utf8"), new String(Array(0x4E8C.asInstanceOf[Char])))
      assertEquals(new String(encode(0x10302), "utf8"), new String(Array(0xD800.asInstanceOf[Char],
                                                                         0xDF02.asInstanceOf[Char])))

      // a client
      val test = "{\"a\":\"\\u0022\"}"
      val Expected = ("a","\"")
      assertTrue(scala.util.parsing.json.JSON.parse(test) match {
        case Some(List(Expected)) => true
        case z => Console.println(z); false
      })
    }
  }

  class SourceTest extends TestCase("Source") {
    def runTest {
	  val s = "Here is a test string"
      val f = io.Source.fromBytes(s.getBytes("utf-8"))
      val b = new collection.mutable.ArrayBuffer[Char]()
      f.copyToBuffer(b)
      assertEquals(s, new String(b.toArray))

      /* todo: same factories for BufferedSource and Source 
       val g = io.BufferedSource.fromBytes(s.getBytes("utf-8"))
       val c = new collection.mutable.ArrayBuffer[Char]()
       g.copyToBuffer(c)
       assertEquals(s, new String(c.toArray))
      */
    }
  }
}
