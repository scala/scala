class C_1 {

  public int f() {
    return 0;
  }
  public C_1 f(int x) {
    return null;
  }
}

class Child extends C_1 {
  @Override
  public C_1 f(int x) {
    return null;
  }
}
