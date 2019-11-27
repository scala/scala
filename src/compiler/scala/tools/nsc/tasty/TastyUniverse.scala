package scala.tools.nsc.tasty

import bridge._

abstract class TastyUniverse extends TastyKernel
  with LoggingOps
  with FlagOps
  with TypeOps
  with ContextOps
  with SymbolOps
  with NameOps
  with TreeOps
