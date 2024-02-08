package lib;

public class InnerClass {

  public static class StaticInner<U> {
    public U innerField;

    public StaticInner(U innerField) {
      this.innerField = innerField;
    }

    public U getInnerField() {
      return innerField;
    }
  }

  public class Inner<U> {
    public U innerField;

    public Inner(U innerField) {
      this.innerField = innerField;
    }

    public U getInnerField() { return innerField; }
  }

  public <U> InnerClass.Inner<U> createInner(U innerField) {
    return new Inner<>(innerField);
  }

  public static <U> InnerClass.Inner<U> createInnerStatic(U innerField) {
    InnerClass innerClass = new InnerClass();
    return innerClass.new Inner<>(innerField);
  }

  public static <U> InnerClass.StaticInner<U> createStaticInnerStatic(U innerField) {
    return new InnerClass.StaticInner<>(innerField);
  }

}
