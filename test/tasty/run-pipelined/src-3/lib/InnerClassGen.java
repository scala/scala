package lib;

public class InnerClassGen<T> {

  public static class StaticInner<U> {
    public U innerField;

    public StaticInner(U innerField) {
      this.innerField = innerField;
    }

    public U getInnerField() { return innerField; }
  }

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

  public <U> InnerClassGen<T>.Inner<U> createInner(T outerField, U innerField) {
    return new Inner<>(outerField, innerField);
  }

  public static <T, U> InnerClassGen<T>.Inner<U> createInnerStatic(T outerField, U innerField) {
    var innerClass = new InnerClassGen<T>();
    return innerClass.new Inner<>(outerField, innerField);
  }

  public static <U> InnerClassGen.StaticInner<U> createStaticInnerStatic(U innerField) {
    return new InnerClassGen.StaticInner<>(innerField);
  }

}
