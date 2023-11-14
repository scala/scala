package test;

public class O2 extends Outer {
  public static class O3 extends O2 {}
  public static class O4 extends Outer {}
  public class O5 extends O4 {}
  public class O6 extends O2 {}
  public class O7 extends O5 {}
}

