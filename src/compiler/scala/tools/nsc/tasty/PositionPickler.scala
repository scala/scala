//package scala.tools.nsc
//package tasty
//
//import ast._
//import ast.Trees._
//import ast.Trees.WithLazyField
//import util.{SourceFile, NoSource}
//import core._
//import Contexts._, Symbols._, Annotations._, Decorators._
//import collection.mutable
//import TastyBuffer._
//import util.Spans._
//import TastyFormat.SOURCE
//
//class PositionPickler(pickler: TastyPickler, addrOfTree: untpd.Tree => Addr) {
//  val buf: TastyBuffer = new TastyBuffer(5000)
//  pickler.newSection("Positions", buf)
//  import ast.tpd._
//
//  private val pickledIndices = new mutable.BitSet
//
//  def header(addrDelta: Int, hasStartDelta: Boolean, hasEndDelta: Boolean, hasPoint: Boolean): Int = {
//    def toInt(b: Boolean) = if (b) 1 else 0
//    (addrDelta << 3) | (toInt(hasStartDelta) << 2) | (toInt(hasEndDelta) << 1) | toInt(hasPoint)
//  }
//
//  def picklePositions(roots: List[Tree])(implicit ctx: Context): Unit = {
//    var lastIndex = 0
//    var lastSpan = Span(0, 0)
//    def pickleDeltas(index: Int, span: Span) = {
//      val addrDelta = index - lastIndex
//      val startDelta = span.start - lastSpan.start
//      val endDelta = span.end - lastSpan.end
//      buf.writeInt(header(addrDelta, startDelta != 0, endDelta != 0, !span.isSynthetic))
//      if (startDelta != 0) buf.writeInt(startDelta)
//      if (endDelta != 0) buf.writeInt(endDelta)
//      if (!span.isSynthetic) buf.writeInt(span.pointDelta)
//      lastIndex = index
//      lastSpan = span
//
//      pickledIndices += index
//    }
//
//    def pickleSource(source: SourceFile): Unit = {
//      buf.writeInt(SOURCE)
//      buf.writeInt(pickler.nameBuffer.nameIndex(source.pathName).index)
//    }
//
//    /** True if x's position shouldn't be reconstructed automatically from its initial span
//     */
//    def alwaysNeedsPos(x: Positioned) = x match {
//      case
//          // initialSpan is inaccurate for trees with lazy field
//          _: WithLazyField[_]
//
//          // A symbol is created before the corresponding tree is unpickled,
//          // and its position cannot be changed afterwards.
//          // so we cannot use the tree initialSpan to set the symbol position.
//          // Instead, we always pickle the position of definitions.
//          | _: Trees.DefTree[_]
//
//          // package defs might be split into several Tasty files
//          | _: Trees.PackageDef[_]
//          // holes can change source files when filled, which means
//          // they might lose their position
//          | _: TreePickler.Hole => true
//      case _ => false
//    }
//
//    def traverse(x: Any, current: SourceFile): Unit = x match {
//      case x: untpd.Tree =>
//        if (x.span.exists) {
//          val addr = addrOfTree(x)
//          if (addr != NoAddr) {
//            if (x.source != current) {
//              // we currently do not share trees when unpickling, so if one path to a tree contains
//              // a source change while another does not, we have to record the position of the tree twice
//              // in order not to miss the source change. Test case is t3232a.scala.
//              pickleDeltas(addr.index, x.span)
//              pickleSource(x.source)
//            }
//            else if (!pickledIndices.contains(addr.index) &&
//                     (x.span.toSynthetic != x.envelope(x.source) || alwaysNeedsPos(x)))
//              pickleDeltas(addr.index, x.span)
//          }
//        }
//        x match {
//          case x: untpd.MemberDef @unchecked => traverse(x.symbol.annotations, x.source)
//          case _ =>
//        }
//        val limit = x.productArity
//        var n = 0
//        while (n < limit) {
//          traverse(x.productElement(n), x.source)
//          n += 1
//        }
//      case y :: ys =>
//        traverse(y, current)
//        traverse(ys, current)
//      case x: Annotation =>
//        traverse(x.tree, current)
//      case _ =>
//    }
//    for (root <- roots) {
//      traverse(root, NoSource)
//    }
//  }
//}
