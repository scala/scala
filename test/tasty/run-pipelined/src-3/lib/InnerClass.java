package lib;

public class InnerClass {

  public class Inner<U> {
    public U innerField;

    public Inner(U innerField) {
      this.innerField = innerField;
    }

    public U getInnerField() { return innerField; }
  }

  public <U> Inner<U> createInner(U innerField) {
    return new Inner<>(innerField);
  }

  public static <U> InnerClass.Inner<U> createInnerStatic(U innerField) {
    var innerClass = new InnerClass();
    return innerClass.new Inner<>(innerField);
  }

}
