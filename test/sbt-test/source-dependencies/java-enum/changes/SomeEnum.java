package pl.typosafe;

/**
 * Author: Krzysztof Romanowski
 */
public enum SomeEnum {
    Baz,
    Bar {
        @Override
        public int foo() {
            return 2;
        }
    };

    public int foo(){
        return 1;
    }
}
