object Test extends App {
  // A variation of SI-6011, which eluded the fix
  // in 2.10.0.
  //
  // duplicate keys in SWITCH, can't pick arbitrarily one of them to evict, see SI-6011.
  // at scala.reflect.internal.SymbolTable.abort(SymbolTable.scala:50)
  // at scala.tools.nsc.Global.abort(Global.scala:249)
  // at scala.tools.nsc.backend.jvm.GenASM$JPlainBuilder$jcode$.emitSWITCH(GenASM.scala:1850)
  ((1: Byte): @unchecked @annotation.switch) match {
    case 1 => 2
    case 1 => 3 // crash
  }
}
