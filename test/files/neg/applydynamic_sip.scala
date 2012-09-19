object Test extends App {
  val qual: Dynamic = ???
  val expr = "expr"
  val a = "a"
  val a2 = "a2"

  qual.sel(a, a2: _*)
  qual.sel(arg = a, a2: _*)
  qual.sel(arg, arg2 = "a2", a2: _*)

  val bad1 = new Dynamic {
    def selectDynamic(n: Int) = n
    def applyDynamic(n: Int) = n
    def applyDynamicNamed(n: Int) = n
    def updateDynamic(n: Int) = n

  }
  bad1.sel
  bad1.sel(1)
  bad1.sel(a = 1)
  bad1.sel = 1

  val bad2 = new Dynamic {
    def selectDynamic = 1
    def applyDynamic = 1
    def applyDynamicNamed = 1
    def updateDynamic = 1
  }
  bad2.sel
  bad2.sel(1)
  bad2.sel(a = 1)
  bad2.sel = 1
}
