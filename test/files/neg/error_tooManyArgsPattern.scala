object Test {
  def test(xs: Any)  = xs match { // test error message TooManyArgsPatternError
    case List(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23) => 7
  }
}
