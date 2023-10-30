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

class JTest {
    Ba t(Ba b) { return b.m(); }
    Ca t(Ca c) { return c.m(); }
    Da t(Da d) { return d.m(); }

    A a = x -> x + 1;

    // error: not a SAM, multiple non-overriding abstract methods found
    // Ba ba = x -> x + 1;
    // Bb bb = x -> x + 1;
    Bc bc = x -> x + 1;
}

// error: does not override abstract method m
// class Xa implements Ba { public int sam(int x) { return x; } }
// class Xb implements Bb { public int sam(int x) { return x; } }

class Xc implements Bc { public int sam(int x) { return x; } }

// error: does not override abstract method m
// class Ya extends Ca { public int sam(int x) { return x; } }
// class Yb extends Cb { public int sam(int x) { return x; } }

// error: does not override abstract method equals
// class Yc extends Cc { public int sam(int x) { return x; } }

// error: does not override abstract method m
// class Za extends Da { public int sam(int x) { return x; } }
// class Zb extends Db { public int sam(int x) { return x; } }

// error: does not override abstract method equals
// class Zc extends Dc { public int sam(int x) { return x; } }
