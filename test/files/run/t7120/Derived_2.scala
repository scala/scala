trait Derived extends Base[ Container ] {
  protected def defect = { case c: Container => c.v.toString }
}

// Erasure was ignoring the prefix `Derived#7001.this` when erasing
// A1, and consequently used `Object` rather than `Container`, which
// was only seen because that signature clashed with the bridge method.
//
// applyOrElse[A1 <: Derived#7001.this.UserType#7318, B1 >: String](x1: A1)
