public abstract class Abstract extends Base {
  // overrides Base#bar under the erasure model
  public void bar(java.util.List<java.lang.Integer> foo) { return; }

  // must force re-implementation in derived classes
  public abstract boolean foo();
}
