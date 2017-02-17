public interface VarargGeneric<T> {
  String genericOne(T x, String args);
  // we cannot annotate this with @SafeVarargs, because
  // it's in an interface. so that's why a warning from
  // javac appears in the checkfile.
  String genericVar(T x, String... args);
}
