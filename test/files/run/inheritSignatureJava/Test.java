class A extends Sub {
  @Override
  public String foo(String a) {
    return "Test-" + a /*+ super.foo(a)*/; // super call doesn't compile
  }
}
class B extends Buk {
  @Override
  public String foo(String a) {
    return "Test-" + a + superFoo(a) /*+ super.foo(a)*/; // super call doesn't compile
  }
}

public class Test {
  public static void main(String[] args) {
    System.out.println(new A().foo("hai"));
    System.out.println(new B().foo("hai"));
  }
}
