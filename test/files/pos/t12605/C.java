public class C {
  public class T1 { public class A {} }
  public void t(T1.A a) {}

  public class T2 { public class A {} }
  public void t(T2.A a) {}

  // https://github.com/scala/bug/issues/12606, requires Java 16+
  // public class U { public static class A {} }
  // public void u(U.A a) {}
}
