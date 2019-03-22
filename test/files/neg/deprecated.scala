// scalac: -Xlint:deprecation -Xfatal-warnings -opt:inline -opt-inline-from:<sources>
//

trait T {
  @deprecated def f = ???

  @deprecated("Don't use it.", since="1.0") def g = ???

  @deprecated("Don't use it."/*, forRemoval=true*/) def stale = ???

  @deprecated("Don't use it.", since="1.0"/*, forRemoval=true*/) def gross = ???

  @deprecated("Don't use it.", since="1.0"/*, forRemoval=true*/) @inline def innie = ???

  @deprecated("Prefer toString instead.", since="1.0"/*, forRemoval=false*/) @inline def keeper = toString()
}

object Main {
  def t: T = ???

  t.f
  t.g
  t.stale
  t.gross

  t.innie   // warn because API will be removed
  t.keeper  // don't warn because it's an inlined forwarder? maybe just warn.
}
