public class Parent<A>{
    class I1 {}
    class I2 extends Parent.I1 {}

    // OKAY:
    class I3 extends I1 {}
    static class I4 {}
    static class I5 extends Parent.I4 {}
}
