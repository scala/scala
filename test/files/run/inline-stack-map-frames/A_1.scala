//> using options -opt:inline:**
class A {
  @noinline final def b: B = null
  @inline final def a: A = b
}

class B extends A
