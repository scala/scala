public class JJ {
    public static void main(String[] args) {
        // Declare anonymous class depending on Scala class
        S s = new S() {
            public void foo(String s) {
                System.out.println(s);
            }
        };
        s.foo("ahoy");
    }
}
