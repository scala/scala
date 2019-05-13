public class Test {
    class D extends example.AbstractPartialFunction<String, String> {
        public boolean isDefinedAt(String s) { return false; }
    };
}