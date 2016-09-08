
object Test extends App {
  val c = new C_1
  assert(c.nulled == "X\u0000ABC")    // "X\000ABC"
  assert(c.supped == "𐒈𐒝𐒑𐒛𐒐𐒘𐒕𐒖")

  assert(C_1.NULLED == "X\u0000ABC")  // "X\000ABC"
  assert(C_1.SUPPED == "𐒈𐒝𐒑𐒛𐒐𐒘𐒕𐒖")

  assert(C_1.NULLED.size == "XYABC".size)
  assert(C_1.SUPPED.codePointCount(0, C_1.SUPPED.length) == 8)
}
