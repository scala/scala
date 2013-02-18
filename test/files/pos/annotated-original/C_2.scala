object Bug {
  M.m {
    def s = ""
    M.m(s): @unchecked // error: macro has not been expanded.
    ???
  }
}
