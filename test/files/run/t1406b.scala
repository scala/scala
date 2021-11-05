
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
  val c, d = C(42)
  println(c + d)
  println(c * d)
  println(c ☀ d)
  println(c * d + d)
  println(c ☀ d + d)
  println(c ☀= d + d)           // assignment op is low precedence
  println(c 𐀀 d + d)            // the first one, letter should be low precedence
  println(c 🌀d + d)            // the second one, cyclone should be high precedence
  println(c 🌀= d + d)            // the second one, cyclone should be high precedence
}

