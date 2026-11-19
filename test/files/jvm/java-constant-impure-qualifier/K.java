public class K {
  public final int inst = 42;
  public final String str = "s";
  public final boolean flag = true;
  public static final int STATIC = 7;

  public static K make() { System.out.println("make"); return null; }
  public static K mk() { System.out.println("mk"); return new K(); }
}
