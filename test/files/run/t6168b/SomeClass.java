public class SomeClass {
    private final Context<SomeClass> context = new Context<SomeClass>();
    {
        context.setParent(this);
    }

    public final Context.Field<Integer> f = context.intField();

}


