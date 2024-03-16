// javaVersion: 16+
record R1(int i, String s) {

    public String someMethod() {
        return s + "!";
    }
}

record R0() {}

record R4<T>(T t) {}
