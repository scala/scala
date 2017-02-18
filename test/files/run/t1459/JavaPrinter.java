public class JavaPrinter implements VarArg {
  public void doit(String... s) {
    System.out.print("JavaPrinter: ");
    for(String str : s)
      System.out.print(str + " ");
  }
}
