
case class C(n: Int) {
  def 𐀀(c: C): C = C(n * c.n)   // actually a letter but supplementary 0x10000
  def ☀(c: C): C = C(n * c.n)   // just a symbol
  def ☀=(c: C): C = C(n * c.n)   // just a symbol
  def 🌀(c: C): C = C(n * c.n)  // cyclone operator is symbol, supplementary
  def 🌀=(c: C): C = C(n * c.n)  // cyclone operator is symbol, supplementary
  def *(c: C): C = C(n * c.n)
  def +(c: C): C = C(n + c.n)
}

object Test extends App {
  val Sum = 84
  val Product = 1764
  val ProductSum = 1806
  val SumProduct = 3528
  val c, d = C(42)
  def assertEquals(expected: Int, actual: C) = assert(expected == actual.n)
  assertEquals(Sum, c + d)
  assertEquals(Product, c * d)
  assertEquals(Product, c ☀ d)
  assertEquals(ProductSum, c * d + d)
  assertEquals(ProductSum, c ☀ d + d)
  assertEquals(SumProduct, c ☀= d + d)           // assignment op is low precedence
  assertEquals(SumProduct, c 𐀀 d + d)            // the first one, letter should be low precedence
  assertEquals(ProductSum, c 🌀d + d)            // the second one, cyclone should be high precedence
  assertEquals(SumProduct, c 🌀= d + d)          // assignment op is low precedence
}
