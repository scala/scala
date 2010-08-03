object Test extends Application {
  object deprNam2 {
    def f(@deprecatedName('s) x: String) = 1
    def f(s: Object) = 2

    def g(@deprecatedName('x) s: Object) = 3
    def g(s: String) = 4
  }

  deprNam2.f(s = new Object)
  deprNam2.f(s = "dlfkj")
  deprNam2.g(x = "dlkjf")
  deprNam2.g(s = new Object)
}
