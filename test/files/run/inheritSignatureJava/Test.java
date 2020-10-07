class A extends Sub {
  @Override
  public String foo(String a) {
    return "(A)" + super.foo(a);
  }
}
class B extends Buk {
  @Override
  public String foo(String a) {
    return "(B)" + superFoo(a) + super.foo(a);
  }
}

public class Test {
  public static void main(String[] args) {
    System.out.println(new A().foo("hai"));
    System.out.println(new B().foo("hai"));
  }
}
