//package scala.tools.nsc
//package tasty
//
//import Contexts._, Decorators._
//import Names.{Name, TermName}
//import StdNames.nme
//import TastyUnpickler._
//import TastyBuffer.NameRef
//import util.Spans.offsetToInt
//import printing.Highlighting._
//
///** Reads the package and class name of the class contained in this TASTy */
//class TastyClassName(bytes: Array[Byte]) {
//
//  val unpickler: TastyUnpickler = new TastyUnpickler(bytes)
//  import unpickler.{nameAtRef, unpickle}
//
//  /** Returns a tuple with the package and class names */
//  def readName(): Option[(TermName, TermName)] = unpickle(new TreeSectionUnpickler)
//
//  class TreeSectionUnpickler extends SectionUnpickler[(TermName, TermName)](TreePickler.sectionName) {
//    import TastyFormat._
//    def unpickle(reader: TastyReader, tastyName: NameTable): (TermName, TermName) = {
//      import reader._
//      def readName() = {
//        val idx = readNat()
//        nameAtRef(NameRef(idx))
//      }
//      def readNames(packageName: TermName): (TermName, TermName) = {
//        val tag = readByte()
//        if (tag >= firstLengthTreeTag) {
//          val len = readNat()
//          val end = currentAddr + len
//          tag match {
//            case TYPEDEF =>
//              val className = readName()
//              goto(end)
//              (packageName, className)
//            case IMPORT | VALDEF =>
//              goto(end)
//              readNames(packageName)
//            case PACKAGE =>
//              readNames(packageName)
//          }
//        }
//        else tag match {
//          case TERMREFpkg | TYPEREFpkg =>
//            val subPackageName = readName()
//            readNames(subPackageName)
//          case _ =>
//            readNames(packageName)
//        }
//      }
//      readNames(nme.EMPTY_PACKAGE)
//    }
//  }
//}
