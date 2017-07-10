class C {
  class D { public int i() { return 2; } }
  static class E { public int i() { return 2; } }
  static class F { static class G { public int i() { return 2; } } }
}

// Test2 has an INNERCLASS attribute for C$D
class Test_2 {
  public static int acceptD(C.D cd) { return cd.i(); }
  public static int acceptE(C.E ce) { return ce.i(); }
  public static int acceptG(C.F.G cg ) { return cg.i(); }
}
