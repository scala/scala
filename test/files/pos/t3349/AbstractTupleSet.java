public abstract class AbstractTupleSet implements TupleSet {
  public void addColumn(String name, Class type) {
    throw new UnsupportedOperationException();
  }

  public void addColumn(String name, String expr) {
    throw new UnsupportedOperationException();
  }
}
