abstract public class Base {
  // This must considered to be overridden by Abstract#foo based
  // on the erased signatures. This special case is handled by
  // `javaErasedOverridingSym` in `RefChecks`.
  public abstract void bar(java.util.List<java.lang.String> foo) { return; }

  // But, a concrete method in a Java superclass must not excuse
  // a deferred method in the Java subclass!
  public boolean foo() { return true; }
}
