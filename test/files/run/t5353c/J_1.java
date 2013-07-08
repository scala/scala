public abstract class J_1<T> implements J_0<T> {
  public T[] javaArray_c() { return javaArray_a(); }
  public abstract T[] javaArray_a();
  public static <U> U[] javaArray_static() { return null; }

  public          T javaArrayGet_cc() { return javaArray_c()[0]; }
  public          T javaArrayGet_ca() { return javaArray_a()[0]; }
  public abstract T javaArrayGet_ac();
  public abstract T javaArrayGet_aa();

  public abstract void javaVarargs_a(T... ts);
  public          void javaVarargs_c(T... ts) { return; }

  private void show(Object msg) { System.out.println(msg); }
}
