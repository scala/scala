public class OuterClass {
    public class InnerClass { }

    public Object getInnerClassInstance() {
        return new InnerClass();
    }
}
