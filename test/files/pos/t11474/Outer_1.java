public class Outer_1 {
    public interface BaseBuilder { }
    public abstract static class Container<B> { }
    public abstract static class Builder<M extends Container<B>, B extends Builder<M, B>> implements BaseBuilder { }
    public abstract static class Builder1<T extends Builder1<T>> extends Builder { }
    public abstract class Builder2 extends Builder1<Builder2> { }

    interface MyBase {
        BaseBuilder newBuilder();
    }

    public static abstract class M1 implements MyBase  {
        public Builder2 newBuilder() { return null; }
    }

    public static abstract class M2 implements MyBase {
        public Builder newBuilder() { return null; }
    }
}
