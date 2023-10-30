interface A {
    default A m() {
        return this;
    }

    int sam(int x);
}

// subinterfaces

interface Ba extends A {
    @Override
    Ba m();
}

interface Bb extends A {
    @Override
    A m();
}

interface Bc extends A {
    @Override
    boolean equals(Object other);
}

// subclasses

abstract class Ca implements A {
    @Override
    public abstract Ca m();
}

abstract class Cb implements A {
    @Override
    public abstract A m();
}

abstract class Cc implements A {
    @Override
    public abstract boolean equals(Object other);
}

// both classes

abstract class Aa {
    public Aa m() {
        return this;
    }

    public abstract int sam(int x);
}

abstract class Da extends Aa {
    @Override
    public abstract Da m();
}

abstract class Db extends Aa {
    @Override
    public abstract Aa m();
}

abstract class Dc extends Aa {
    @Override
    public abstract boolean equals(Object other);
}
