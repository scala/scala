// check that the `var x` below is assigned the type `Int`, and not `Constant(0)`,
// and that we can assign to it (if it gets a constant type, the `x` in `x = 42`
// is constant-folded to `0` and we can't find a setter..
trait C { protected final var x = 0; x = 42 }
