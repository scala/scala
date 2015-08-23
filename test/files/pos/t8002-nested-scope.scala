//  This test serves to capture the status quo, but should really
// emit an accessibility error.

// `Namers#companionSymbolOf` seems too lenient, and currently doesn't
// implement the same-scope checks mentioned:
//
// https://github.com/scala/scala/pull/2816#issuecomment-22555206
//
class C {
  def foo = {
    class C { private def x = 0 }

    {
      val a = 0
      object C {
        new C().x
      }
    }
  }
}
