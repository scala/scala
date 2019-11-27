package scala.tools.nsc.tasty

import scala.tools.nsc.tasty.bridge._

trait TastyUniverse extends TastyKernel
  with LoggingOps
  with FlagOps
  with TypeOps
  with ContextOps
  with SymbolOps
  with NameOps
  with TreeOps
