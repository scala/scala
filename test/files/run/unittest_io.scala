import testing.SUnit._

object Test extends TestConsoleMain {
  def suite = new TestSuite(new UTF8Tests())

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
      assertTrue(util.parsing.json.JSON.parse(test) match {
        case Some(List(Expected)) => true
        case z => Console.println(z); false
      })
    }
  }
}
