package java_raw_parent;

import java.io.Serializable;

interface A<T extends Serializable> {}
interface B<T> extends Serializable {}
class C implements A<B> { // did you mean A<B<?>>? Java doesn't care...
    static void test(A<?> a) {}
}
