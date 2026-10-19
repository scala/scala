package p;

// `Square` is loaded from its classfile, nothing else registers it as a child of `Poly`
sealed interface Poly extends Shape permits Square {}
