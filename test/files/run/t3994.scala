trait T {
  trait Default { def foo = this }
  object Default extends Default
}

class Crash { // if you change this to a `trait` it keeps failing, though if it is an `object` it compiles just fine!
  class Element

  /* declare this as a class, and the crash goes away */
  trait ElementOrdering extends Ordering[Element] {
    def compare(a: Element, b: Element): Int = 0
  }

  implicit object ElementOrdering extends ElementOrdering
}

object Test extends App {
  (new T {}).Default
  (new Crash).ElementOrdering
}
