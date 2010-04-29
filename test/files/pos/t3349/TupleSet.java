public interface TupleSet {
    public void addColumn(String name, Class type);
    public void addColumn(String name, String expr);
}