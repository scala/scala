public class Test {
  public static void main(String[] args) {
    AbstractTrav<String> lsSharp1 = new C1().tail();

    // Object is the result type for the static forwarder (might be because of #11305)
    Object lsSharp2 = O.tail();

    AbstractTrav<String> lsSharp3 = new C2<String>().tail();
  }
}
