object O {
  // error: C is not a legal prefix for a constructor
  C().CC()
  // but this works.
  D().DD()
}

case class C() {
  case class CC()
}

case class D() {
  class DD()
  object DD {
    def apply() = new DD()
  }
}
