class JavaClassWithCheckedExceptions_1<E1 extends Exception>  {
  public JavaClassWithCheckedExceptions_1() throws NullPointerException {}

  public void bar() throws E1 {}
  public void baz(int x) throws IllegalStateException {}
  public <E2 extends Exception> void foo() throws E2 {}
}