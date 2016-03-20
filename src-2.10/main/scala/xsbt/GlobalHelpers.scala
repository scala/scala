package xsbt

import scala.tools.nsc.Global

trait GlobalHelpers {
  val global: CallbackGlobal
  import global._

  def symbolsInType(tp: Type): Set[Symbol] = {
    val typeSymbolCollector =
      new CollectTypeCollector({
        case tpe if (tpe != null) && !tpe.typeSymbolDirect.hasPackageFlag => tpe.typeSymbolDirect
      })

    typeSymbolCollector.collect(tp).toSet
  }
}
