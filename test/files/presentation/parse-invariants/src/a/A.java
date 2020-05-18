package syntax;

@NoArgs
@Empty()
@Simple(n = 1, c = '2', f = 6.7f, d = 8.9, s = "t", z = A.class, e = P.Pluto, a = @C.I("t"))
@Arrays({ @Array({0, 1, C._2}), @Array(3) })
@Deprecated
class A {
  transient volatile int x;
  strictfp void test() {
  }

  native void nativeMethod()

  synchronized void syncMethod() {}

  void thrower() throws Throwable {}

  @Deprecated void deprecated() {}
}

strictfp class B {}