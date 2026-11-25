// Known limitation: looking up String completes the parents of p, which needs this import
// to decide whether Predef is masked, and typing the import needs the parents of p.
object p extends Ordering[String] { def compare(a: String, b: String) = 0; object Predef }

import p.Predef.{wait => _}
