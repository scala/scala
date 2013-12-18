object Test {
  Macro {
    def s = ""
    Macro(s): @unchecked
    ???
  }
}
// Was: a range position validation error (unpositioned tree)