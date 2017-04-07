public class Test {
  public static void main(String[] args) {
    VarArg jp = new JavaPrinter();
    VarArg ip = new InheritingPrinter();
    VarArg sp = new ScalaPrinter();
    doYourThing(jp);
    doYourThing(ip);
    doYourThing(sp);
  }

  public static void doYourThing(VarArg va) {
    va.doit("one", "two", "three");
    System.out.println();
  }
}
