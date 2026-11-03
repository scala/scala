package p

// Checking the match needs the children of `Shape`, while `S`, one of them, is still being completed.
final class S extends Shape with Tag[M.v.type]

object M { val v = (null: Shape) match { case _: Other => 0; case _ => 1 } }
