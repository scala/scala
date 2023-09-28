public class JJ {
	public static void main(String[] args) {
    // Declare anonymous class depending on Scala class
    class FromScala extends S {
      public void foo(String s) {
        System.out.println(s);
      }
    }
    S s = new FromScala();
    s.foo("ahoy");
	}
}
