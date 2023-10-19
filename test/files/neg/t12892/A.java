interface A {
    default A m() {
        return this;
    }
}

interface B extends A {
    @Override
    B m();
}

class Test {
  B t(B b) { return b.m(); }
}
