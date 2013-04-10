public class JavaTest {
  public static void main(String[] args) {
    SomeClass a = new SomeClass();
    SomeClass2 a2 = new SomeClass2();
    SomeClass b = a.f.set(23).f.set(23);
    SomeClass2 b2 = a2.f.set(23).f.set(23);
  }
}