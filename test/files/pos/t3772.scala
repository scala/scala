class Test {
  def m = {
    case class C(c: Int)
    object C { def xxx = true}
    C(42).c
    C.xxx
  }
}
