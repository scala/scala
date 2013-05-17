public class Context<ParentType> {
    private ParentType parent;

    public Context() {}

    public ParentType getParent() {
        return parent;
    }

    public void setParent(ParentType parent) {
         this.parent = parent;
    }

    public Field<Integer> intField() {
        return new Field<Integer>() {
            @Override
            public Integer get() {
                return 0;
            }

            @Override
            public ParentType set(Integer t) {
                return parent;
            }
        };
    }

    public static abstract class Field<T> {

        public abstract T get();

        public abstract Object set(T t);
    }
}