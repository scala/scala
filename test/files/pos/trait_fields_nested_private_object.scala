trait NestedObj {
  private object O { println("NO") }
}


class C extends NestedObj {
  def O = ???
}