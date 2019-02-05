package syntax;

class A {
  transient volatile int x;
  strictfp void test() {
  }

  native void nativeMethod()

  synchronized void syncMethod() {}

  void thrower() throws Throwable {}

}

strictfp class B {}