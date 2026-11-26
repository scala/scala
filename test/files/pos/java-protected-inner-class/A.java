package a;
public class A {
  // the default constructors would be protected (JLS 8.8.9), and not accessible in `new I` in B.scala
  protected class I { public I() {} }
  protected static class S { public S() {} }
}
