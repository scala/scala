class C { def foo = 0 }
class D extends C { private def foo[A] = 0 }

class E { private def foo = 0 }
class F extends E { def foo[A] = 0 } // okay
