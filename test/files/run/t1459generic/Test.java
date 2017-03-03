public class Test {
  public static void main(String[] args) throws Exception {
    VarargGeneric vg = new Impl();
    System.out.println(vg.genericOne("a", "b"));
    System.out.println(vg.genericVar("a", "b"));
    // should not result in java.lang.AbstractMethodError: Impl.genericVar(Ljava/lang/Object;[Ljava/lang/String;)Ljava/lang/String;
    // --> genericVar needs a varargs bridge (scala -> java varargs) and a standard generics bridge
    // (for comparison, including genericOne, which needs only a generics bridge)
  }
}
