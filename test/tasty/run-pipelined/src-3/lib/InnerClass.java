package lib;

public class InnerClass<T> {

  public class Inner<U> {
    public T outerField;
    public U innerField;

    public Inner(T outerField, U innerField) {
      this.outerField = outerField;
      this.innerField = innerField;
    }

    public T getOuterField() { return outerField; }
    public U getInnerField() { return innerField; }
  }

}
