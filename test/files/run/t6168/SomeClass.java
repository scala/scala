public class SomeClass {
    private final Context<SomeClass> context = new Context<SomeClass>();
    {
        context.setParent(this);
    }

    public final Context<SomeClass>.Field<Integer> f = context.intField();

    public SomeClass() {
        f.set(23).f.set(23);
    }
}


