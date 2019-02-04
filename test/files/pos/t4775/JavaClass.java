public class JavaClass {
    public static class Element {

    }

    public static <T extends Element> int foo(Element a, Class<T> b, boolean c, Class<? extends T>... d) {
        return 1;
    }

    public static <T extends Element> int foo(Element a, Class<? extends T> b, boolean c) {
        return 2;
    }

    public static <T extends Element> int foo(Element a, Class<? extends T>... b) {
        return 3;
    }

    public static <T extends Element> int foo(Element a, boolean b, Class<? extends T>... c) {
        return 4;
    }

    static {
        foo(new Element(), Element.class, false);
    }
}
