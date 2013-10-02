class Outer<E> {
  abstract class Inner {
    abstract public void foo(E e);
  }
}

class Child extends Outer<String> {
  // the implicit prefix for Inner is Outer<E> instead of Outer<String>
  public Inner getInner() {
    return new Inner() {
     public void foo(String e) { System.out.println("meh "+e); }
    };
  }
}
