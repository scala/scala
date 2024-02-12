// this test ensures that Object can accept Any from Scala
// see Definitions.ObjectTpeJava
package lib;

@SuppressWarnings("unchecked")
public class ObjectTpeJava {

  public static class Inner<T> extends Object {
    public T field1;
    public T getter1() { return field1; }
    public Object field2;
    public Object getter2() { return field2; }

    public Inner(T param1, Object param2) {
      this.field1 = param1;
      this.field2 = param2;
    }

    public void meth1(T arg) {}
    public <U extends T> void meth2(U arg) {}
  }

  // 1. At the top level:
  public void meth1(Object arg) {}
  public <T> void meth2(T arg) {} // T implicitly extends Object

  // 2. In a class type argument:
  public void meth3(scala.collection.immutable.List<Object> arg) {}
  public <T> void meth4(scala.collection.immutable.List<T> arg) {}

  // 3. As the type argument of an array:
  public void meth5(Object[] arg) {}
  public <T> void meth6(T[] arg) {}

  // 4. As the repeated argument of a varargs method:
  public void meth7(Object... args) {}
  public <T> void meth8(T... args) {}
}
