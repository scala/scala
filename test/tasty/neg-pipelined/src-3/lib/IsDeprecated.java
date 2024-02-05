package lib;

public class IsDeprecated {

    @Deprecated
    public static int foo() { return 1; }

    @Deprecated
    public int bar() { return 1; }

    @Deprecated
    public static class Inner {
        public static int baz() { return 1; }
    }

}
