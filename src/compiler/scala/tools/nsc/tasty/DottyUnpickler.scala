//package scala.tools.nsc
//package tasty
//
//import Contexts._, SymDenotations._,  Decorators._
//import dotty.tools.dotc.ast.tpd
//import TastyUnpickler._
//import classfile.ClassfileParser
//import Names.SimpleName
//import TreeUnpickler.UnpickleMode
//
//object DottyUnpickler {
//
//  /** Exception thrown if classfile is corrupted */
//  class BadSignature(msg: String) extends RuntimeException(msg)
//
//  class TreeSectionUnpickler(posUnpickler: Option[PositionUnpickler], commentUnpickler: Option[CommentUnpickler])
//  extends SectionUnpickler[TreeUnpickler](TreePickler.sectionName) {
//    def unpickle(reader: TastyReader, nameAtRef: NameTable): TreeUnpickler =
//      new TreeUnpickler(reader, nameAtRef, posUnpickler, commentUnpickler, Seq.empty)
//  }
//
//  class PositionsSectionUnpickler extends SectionUnpickler[PositionUnpickler]("Positions") {
//    def unpickle(reader: TastyReader, nameAtRef: NameTable): PositionUnpickler =
//      new PositionUnpickler(reader, nameAtRef)
//  }
//
//  class CommentsSectionUnpickler extends SectionUnpickler[CommentUnpickler]("Comments") {
//    def unpickle(reader: TastyReader, nameAtRef: NameTable): CommentUnpickler =
//      new CommentUnpickler(reader)
//  }
//}
//
///** A class for unpickling Tasty trees and symbols.
// *  @param bytes         the bytearray containing the Tasty file from which we unpickle
// *  @param mode          the tasty file contains package (TopLevel), an expression (Term) or a type (TypeTree)
// */
//class DottyUnpickler(bytes: Array[Byte], mode: UnpickleMode = UnpickleMode.TopLevel) extends ClassfileParser.Embedded with tpd.TreeProvider {
//  import tpd._
//  import DottyUnpickler._
//
//  val unpickler: TastyUnpickler = new TastyUnpickler(bytes)
//  private val posUnpicklerOpt = unpickler.unpickle(new PositionsSectionUnpickler)
//  private val commentUnpicklerOpt = unpickler.unpickle(new CommentsSectionUnpickler)
//  private val treeUnpickler = unpickler.unpickle(treeSectionUnpickler(posUnpicklerOpt, commentUnpicklerOpt)).get
//
//  /** Enter all toplevel classes and objects into their scopes
//   *  @param roots          a set of SymDenotations that should be overwritten by unpickling
//   */
//  def enter(roots: Set[SymDenotation])(implicit ctx: Context): Unit =
//    treeUnpickler.enter(roots)
//
//  protected def treeSectionUnpickler(posUnpicklerOpt: Option[PositionUnpickler], commentUnpicklerOpt: Option[CommentUnpickler]): TreeSectionUnpickler = {
//    new TreeSectionUnpickler(posUnpicklerOpt, commentUnpicklerOpt)
//  }
//
//  protected def computeRootTrees(implicit ctx: Context): List[Tree] = treeUnpickler.unpickle(mode)
//
//  private[this] var ids: Array[String] = null
//
//  override def mightContain(id: String)(implicit ctx: Context): Boolean = {
//    if (ids == null)
//      ids =
//        unpickler.nameAtRef.contents.toArray.collect {
//          case name: SimpleName => name.toString
//        }.sorted
//    ids.binarySearch(id) >= 0
//  }
//}
