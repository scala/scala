
// skalac: -Werror -Xsource:3

//> using options -Werror, "-Wconf:cat=deprecation:e,cat=feature:s", -Xlint , -Xsource:3

class C {
  def f = 42
}

class D {
  def f = 27
  class E extends C {
    def g = f
  }
}
