class Increment {

  // `iinc`
  def increment(x: Int): Int = {
    var i = x
    i += 1
    i += 54
    i += 127
    i -= 1
    i -= 54
    i -= 128
    i
  }

  // `wide iinc`
  def wideIncrement(x: Int): Int = {
    var i = x
    i += 128
    i += 8765
    i += 32767
    i -= 129
    i -= 8765
    i -= 32768
    i
  }

  def tooBigForIinc(x: Int): Int = {
    var i = x
    i += 32768
    i += 56789
    i += 2147483647
    i -= 32769
    i -= 56789
    i -= 2147483647
    i
  }
}
