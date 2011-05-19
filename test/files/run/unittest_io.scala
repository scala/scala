object Test {

  def main(args: Array[String]) {
    UTF8Tests.run()
    SourceTest.run()
  }

  object UTF8Tests {
    import io.UTF8Codec.encode
    def run() {
      assert(new String( encode(0x004D), "utf8") == new String(Array(0x004D.asInstanceOf[Char])))
      assert(new String( encode(0x0430), "utf8") == new String(Array(0x0430.asInstanceOf[Char])))
      assert(new String( encode(0x4E8C), "utf8") == new String(Array(0x4E8C.asInstanceOf[Char])))
      assert(new String(encode(0x10302), "utf8") == new String(Array(0xD800.asInstanceOf[Char],
                                                                     0xDF02.asInstanceOf[Char])))
      // a client
      val test = "{\"a\":\"\\u0022\"}"
      val Expected = ("a","\"")
      assert(scala.util.parsing.json.JSON.parse(test) match {
        case Some(List(Expected)) => true
        case z => Console.println(z); false
      })
    }
  }

  object SourceTest {
    def run() {
      val s = "Here is a test string"
      val f = io.Source.fromBytes(s.getBytes("utf-8"))
      val b = new collection.mutable.ArrayBuffer[Char]()
      f.copyToBuffer(b)
      assert(s == new String(b.toArray))
    }
  }
}
