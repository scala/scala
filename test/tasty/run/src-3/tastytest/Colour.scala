package tastytest

enum Colour {
  case Red, Green, Blue
  case RGB(r: Int, g: Int, b: Int)
  case CMYK(c: Int, m: Int, y: Int, k: Int)
}
