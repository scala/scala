// Looking up Serializable through the `scala` root import must not complete the import below,
// which needs the parents of p.
object p extends Serializable { object Predef { def assert(b: Boolean) = () } }

import p.Predef.{assert}

class C { def f = assert(true) }
