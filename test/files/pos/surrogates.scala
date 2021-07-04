
// allow supplementary chars in identifiers

class 𐐀 {
  def 𐐀 = 42

  // regression check: anything goes in strings
  def x = "𐐀"
  def y = s"$𐐀"
  def w = s" 𐐀"
}

case class 𐐀𐐀(n: Int) {
  def 𐐀𐐀 = n
  def `𐐀𐐀1` = n + n
}

// uncontroversially, orphan surrogates may be introduced
// via unicode escape.
class Construction {
  def hi = '\ud801'
  def lo = '\udc00'
  def endhi = "abc\ud801"
  def startlo = "\udc00xyz"
  def reversed = "xyz\udc00\ud801abc"
}

// was: error: illegal character '\ud801', '\udc00'
