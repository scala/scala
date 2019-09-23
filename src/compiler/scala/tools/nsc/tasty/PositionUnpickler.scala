package scala.tools.nsc
package tasty

//import util.Spans._
//import collection.{mutable, Map}
import TastyRefs.NameRef
//import TastyFormat.SOURCE

/** Unpickler for tree positions */
class PositionUnpickler(reader: TastyReader, nameAtRef: NameRef => TastyUnpickler.TermName) {
//  import reader._
//
//  private var mySpans: mutable.HashMap[Addr, Span] = _
//  private var mySourcePaths: mutable.HashMap[Addr, String] = _
//  private var isDefined = false
//
//  def ensureDefined(): Unit =
//    if (!isDefined) {
//      mySpans = new mutable.HashMap[Addr, Span]
//      mySourcePaths = new mutable.HashMap[Addr, String]
//      var curIndex = 0
//      var curStart = 0
//      var curEnd = 0
//      while (!isAtEnd) {
//        val header = readInt()
//        if (header == SOURCE) {
//          val path = nameAtRef(readNameRef()).toString
//          mySourcePaths(Addr(curIndex)) = path
//        }
//        else {
//          val addrDelta = header >> 3
//          val hasStart = (header & 4) != 0
//          val hasEnd = (header & 2) != 0
//          val hasPoint = (header & 1) != 0
//          curIndex += addrDelta
//          assert(curIndex >= 0)
//          if (hasStart) curStart += readInt()
//          if (hasEnd) curEnd += readInt()
//          mySpans(Addr(curIndex)) =
//            if (hasPoint) Span(curStart, curEnd, curStart + readInt())
//            else Span(curStart, curEnd)
//          }
//      }
//      isDefined = true
//    }
//
//  private[tasty] def spans: Map[Addr, Span] = {
//    ensureDefined()
//    mySpans
//  }
//
//  private[tasty] def sourcePaths: Map[Addr, String] = {
//    ensureDefined()
//    mySourcePaths
//  }
//
//  def spanAt(addr: Addr): Span = spans.getOrElse(addr, NoSpan)
//  def sourcePathAt(addr: Addr): String = sourcePaths.getOrElse(addr, "")
}
