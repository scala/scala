object TestObject {

  def m(i: Int): AnyRef = i match {
    case 0 => new C()
    case 1 => Some(E.A).getOrElse("")
  }

  class C extends Ordered[C] {
    def compare(that: C): Int = ???
  }

  object E extends Enumeration {
    type CharacterClass = Value
    val A = Value
  }
}
