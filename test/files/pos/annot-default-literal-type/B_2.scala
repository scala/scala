// The default arguments of `lit` are inlined into its uses. Read from the pickle, they must keep their literal types.
class B {
  @lit def f = 1
  @lit(s = "a") def g = 2
  def h: Int @lit = 3
}
