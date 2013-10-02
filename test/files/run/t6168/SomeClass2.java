public class SomeClass2 {
    private final Context<SomeClass2> context = new Context<SomeClass2>();
    {
        context.setParent(this);
    }

    public final Context<SomeClass2>.Field<Integer> f = context.intField();

    public SomeClass2() {
        f.set(23).f.set(23);
    }
}