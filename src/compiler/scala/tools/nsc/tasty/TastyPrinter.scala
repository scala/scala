//package scala.tools.nsc
//package tasty
//
//import Contexts._, Decorators._
//import Names.Name
//import TastyUnpickler._
//import TastyBuffer.{Addr, NameRef}
//import util.Spans.offsetToInt
//import printing.Highlighting._
//
//class TastyPrinter(bytes: Array[Byte])(implicit ctx: Context) {
//
//  private[this] val sb: StringBuilder = new StringBuilder
//
//  val unpickler: TastyUnpickler = new TastyUnpickler(bytes)
//  import unpickler.{nameAtRef, unpickle}
//
//  def nameToString(name: Name): String = name.debugString
//
//  def nameRefToString(ref: NameRef): String = nameToString(nameAtRef(ref))
//
//  def printNames(): Unit =
//    for ((name, idx) <- nameAtRef.contents.zipWithIndex) {
//      val index = nameColor("%4d".format(idx))
//      sb.append(index).append(": ").append(nameToString(name)).append("\n")
//    }
//
//  def printContents(): String = {
//    sb.append("Names:\n")
//    printNames()
//    sb.append("\n")
//    sb.append("Trees:\n")
//    unpickle(new TreeSectionUnpickler) match {
//      case Some(s) => sb.append(s)
//      case _ =>
//    }
//    sb.append("\n\n")
//    unpickle(new PositionSectionUnpickler) match {
//      case Some(s) => sb.append(s)
//      case _ =>
//    }
//    sb.append("\n\n")
//    unpickle(new CommentSectionUnpickler) match {
//      case Some(s) => sb.append(s)
//      case _ =>
//    }
//    sb.result
//  }
//
//  class TreeSectionUnpickler extends SectionUnpickler[String](TreePickler.sectionName) {
//    import TastyFormat._
//
//    private[this] val sb: StringBuilder = new StringBuilder
//
//    def unpickle(reader: TastyReader, tastyName: NameTable): String = {
//      import reader._
//      var indent = 0
//      def newLine() = {
//        val length = treeColor("%5d".format(index(currentAddr) - index(startAddr)))
//        sb.append(s"\n $length:" + " " * indent)
//      }
//      def printNat() = sb.append(treeColor(" " + readNat()))
//      def printName() = {
//        val idx = readNat()
//        sb.append(nameColor(" " + idx + " [" + nameRefToString(NameRef(idx)) + "]"))
//      }
//      def printTree(): Unit = {
//        newLine()
//        val tag = readByte()
//        sb.append(" ").append(astTagToString(tag))
//        indent += 2
//        if (tag >= firstLengthTreeTag) {
//          val len = readNat()
//          sb.append(s"(${lengthColor(len.toString)})")
//          val end = currentAddr + len
//          def printTrees() = until(end)(printTree())
//          tag match {
//            case RENAMED =>
//              printName(); printName()
//            case VALDEF | DEFDEF | TYPEDEF | TYPEPARAM | PARAM | NAMEDARG | BIND =>
//              printName(); printTrees()
//            case REFINEDtype | TERMREFin | TYPEREFin =>
//              printName(); printTree(); printTrees()
//            case RETURN | HOLE =>
//              printNat(); printTrees()
//            case METHODtype | ERASEDMETHODtype |
//                 GIVENMETHODtype | ERASEDGIVENMETHODtype | IMPLICITMETHODtype |
//                 POLYtype | TYPELAMBDAtype =>
//              printTree()
//              until(end) { printName(); printTree() }
//            case PARAMtype =>
//              printNat(); printNat()
//            case _ =>
//              printTrees()
//          }
//          if (currentAddr != end) {
//            sb.append(s"incomplete read, current = $currentAddr, end = $end\n")
//            goto(end)
//          }
//        }
//        else if (tag >= firstNatASTTreeTag) {
//          tag match {
//            case IDENT | IDENTtpt | SELECT | SELECTtpt | TERMREF | TYPEREF | SELFDEF => printName()
//            case _ => printNat()
//          }
//          printTree()
//        }
//        else if (tag >= firstASTTreeTag)
//          printTree()
//        else if (tag >= firstNatTreeTag)
//          tag match {
//            case TERMREFpkg | TYPEREFpkg | STRINGconst | IMPORTED => printName()
//            case _ => printNat()
//          }
//        indent -= 2
//      }
//      sb.append(i"start = ${reader.startAddr}, base = $base, current = $currentAddr, end = $endAddr\n")
//      sb.append(s"${endAddr.index - startAddr.index} bytes of AST, base = $currentAddr\n")
//      while (!isAtEnd) {
//        printTree()
//        newLine()
//      }
//      sb.result
//    }
//  }
//
//  class PositionSectionUnpickler extends SectionUnpickler[String]("Positions") {
//
//    private[this] val sb: StringBuilder = new StringBuilder
//
//    def unpickle(reader: TastyReader, tastyName: NameTable): String = {
//      sb.append(s" ${reader.endAddr.index - reader.currentAddr.index}")
//      val spans = new PositionUnpickler(reader, tastyName).spans
//      sb.append(s" position bytes:\n")
//      val sorted = spans.toSeq.sortBy(_._1.index)
//      for ((addr, pos) <- sorted) {
//        sb.append(treeColor("%10d".format(addr.index)))
//        sb.append(s": ${offsetToInt(pos.start)} .. ${pos.end}\n")
//      }
//      sb.result
//    }
//  }
//
//  class CommentSectionUnpickler extends SectionUnpickler[String]("Comments") {
//
//    private[this] val sb: StringBuilder = new StringBuilder
//
//    def unpickle(reader: TastyReader, tastyName: NameTable): String = {
//      sb.append(s" ${reader.endAddr.index - reader.currentAddr.index}")
//      val comments = new CommentUnpickler(reader).comments
//      sb.append(s" comment bytes:\n")
//      val sorted = comments.toSeq.sortBy(_._1.index)
//      for ((addr, cmt) <- sorted) {
//        sb.append(treeColor("%10d".format(addr.index)))
//        sb.append(s": ${cmt.raw} (expanded = ${cmt.isExpanded})\n")
//      }
//      sb.result
//    }
//  }
//
//  protected def nameColor(str: String): String = Magenta(str).show
//  protected def treeColor(str: String): String = Yellow(str).show
//  protected def lengthColor(str: String): String = Cyan(str).show
//}
