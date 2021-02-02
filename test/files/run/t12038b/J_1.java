package p1;

public class J_1 {
    public static abstract class O<A> {
        // non static, runtime reflection should use the generic owner type
        // in the type signature below.
        //
        // also doing for static inner classes runs into cyclic errors (see t12038a.scala)
        // in the current implementation of runtime reflection.
        //
        // This is fine as Java rejects the type selections in `notValidJava` below with:
        //
        //   "cannot select a static class from a parameterized type"
        public abstract class I<B extends A> {}

        public abstract static class J<B> {}
    }
    static void test(O<Object>.I<String> oi) {}

    // static void notValidJava(O<Object>.J<String> oj) {}
}
