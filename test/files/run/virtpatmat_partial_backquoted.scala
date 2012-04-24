object Test extends App {
  class Region { override def toString = "You got me!" }
  class SymbolType
  case class SymbolInfo(tp: SymbolType, regions: List[Region], x: Any)

  def findRegionsWithSymbolType(rawSymbolInfos: Seq[SymbolInfo], symbolType: SymbolType): Set[Region] =
    rawSymbolInfos.collect { case SymbolInfo(`symbolType`, regions, _) => regions }.flatten.toSet

  val stp = new SymbolType
  val stp2 = new SymbolType
  println(findRegionsWithSymbolType(List(SymbolInfo(stp2, List(), null), SymbolInfo(stp, List(new Region), null)), stp))
}