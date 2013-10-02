public class Outer<E> {
    public void elements() {
        new C<E>() {
        };
    }

    private Outer(String a) {}

    static class SubSelf extends Outer<String> {
        public SubSelf() { super(""); }
    }

    private class PrivateInner {
    }
    class SubPrivateInner extends PrivateInner {
    }

    private class PublicInner {
        private PublicInner(String a) {}
    }
    class SubPublicInner extends PublicInner {
        public SubPublicInner() { super(""); }
    }

    private static class PrivateStaticInner {
    }
    public static class SubPrivateStaticInner extends PrivateStaticInner {
    }
}

class C<E> {}
