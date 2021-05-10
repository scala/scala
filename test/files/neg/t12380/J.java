package p;

public class J {
  public static class C {
    public Object m() { return new Object(); }
  }
  public interface I {
    public String m();
  }

  public static class Test extends C implements I {
    @Override public String m() { return ""; }
   }
}
