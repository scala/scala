
package example;

interface A {
    @Deprecated
    default String a() {
        return "a";
    }
}
