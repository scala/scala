public class A_1 {
    public static abstract class X<T> {}

    public static abstract class Y {
        public X appendFile(String s) { return null; }
    }

    public static class Z extends Y {
        @Override
        public X appendFile(String s) { return null; }
    }
}
