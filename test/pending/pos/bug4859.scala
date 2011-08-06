object O {
  C().CC()
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
