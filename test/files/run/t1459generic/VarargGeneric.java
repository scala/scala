public interface VarargGeneric<T> {
  String genericOne(T x, String args);
  String genericVar(T x, String... args);
}
