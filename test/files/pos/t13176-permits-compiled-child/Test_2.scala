package p

// `Shape` and `Other` come from classfiles, so Checkable computes the children of `Shape` while
// typing the match. `S` is being completed (its parents need `M.v.type`), its parents are not known yet.
final class S extends Shape with Tag[M.v.type]

object M { val v = (null: Shape) match { case _: Other => 0; case _ => 1 } }
