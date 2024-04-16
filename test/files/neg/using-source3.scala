
// skalac: -Werror -Xsource:3

//> using options -Werror
//> using options -Xsource:3

class C {
  def f = 42
}

class D {
  def f = 27
  class E extends C {
    def g = f
  }
}
