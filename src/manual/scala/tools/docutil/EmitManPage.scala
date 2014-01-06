/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Stephane Micheloud
 * Adapted from Lex Spoon's sbaz manual
 */

package scala.tools.docutil

// For help on man pages see:
// - http://www.linuxfocus.org/English/November2003/article309.shtml
// - http://www.schweikhardt.net/man_page_howto.html

object EmitManPage {
  import ManPage._

  val out = Console

  def escape(text: String) =
    text.replaceAll("-", "\\-")

  def emitSection(section: Section, depth: Int) {
    def emitPara(text: AbstractText) {
      emitText(text)
      out println "\n.IP"
    }
    def emitText(text: AbstractText) {
      text match {
        case seq:SeqText =>
          seq.components foreach emitText

        case seq:SeqPara =>
          seq.components foreach emitPara

        case Text(text) =>
          out print escape(text)

        case BSlash =>
          out print "\\e"

        case NDash | MDash =>
          out print "\\-"

        case Bold(text) =>
          out print "\\fB"
          emitText(text)
          out print "\\fR"

        case Italic(text) =>
          out print "\\fI"
          emitText(text)
          out print "\\fR"

        case Emph(text) =>
          out.print("\\fI")
          emitText(text)
          out.print("\\fI")

        case Mono(text) =>
          out.print("")
          emitText(text)
          out.print("")

        case Quote(text) =>
          out.print("\"")
          emitText(text)
          out.print("\"")

        case DefinitionList(definitions @ _*) =>
          var n = definitions.length
          for (d <- definitions) {
            out println ".TP"
            emitText(d.term)
            out.println
            emitText(d.description)
            if (n > 1) { out.println; n -= 1 }
          }

        case Link(label, url) =>
          emitText(label)

        case _ =>
          sys.error("unknown text node: " + text)
      }
    }

    def emitParagraph(para: Paragraph) {
      para match {
        case TextParagraph(text) =>
          out println ".PP"
          emitText(text)
          out.println

        case BlockQuote(text) =>
          out println ".TP"
          emitText(text)
          out.println

        case CodeSample(text) =>
          out println "\n.nf"
          out.print(text)
          out println "\n.fi"

        case lst:BulletList =>
          for (item <- lst.items) {
            out println ".IP"
            emitText(item)
            out.println
          }

        case lst:NumberedList =>
          for {
            idx <- List.range(0, lst.items.length)
          } {
            val item = lst.items(idx)
            out.println(".IP \"   " + (idx+1) + ".\"")
            emitText(item)
            out.println
          }

        case TitledPara(title, text) =>
          out println ".PP"
          out print "\\fB"
          emitText(title)
          out print "\\fR"
          emitText(text)

        case EmbeddedSection(sect) =>
          emitSection(sect, depth + 1)

        case _ =>
          sys.error("unknown paragraph node: " + para)
      }
    }

    out println ".\\\""
    out.println(".\\\" ############################## " + section.title + " ###############################")
    out println ".\\\""
    val tag = if (depth > 1) ".SS" else ".SH"
    val title =
      if (section.title.indexOf(" ") > 0) "\"" + section.title + "\""
      else section.title
    out.println(tag + " " + title)

    section.paragraphs foreach emitParagraph
  }

  def emitDocument(doc: Document) {
    out println ".\\\" ##########################################################################"
    out println ".\\\" #                      __                                                #"
    out println ".\\\" #      ________ ___   / /  ___     Scala 2 On-line Manual Pages          #"
    out println ".\\\" #     / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL              #"
    out println ".\\\" #   __\\ \\/ /__/ __ |/ /__/ __ |                                          #"
    out println ".\\\" #  /____/\\___/_/ |_/____/_/ | |    http://scala-lang.org/                #"
    out println ".\\\" #                           |/                                           #"
    out println ".\\\" ##########################################################################"
    out println ".\\\""
    out println ".\\\" Process this file with nroff -man scala.1"
    out println ".\\\""
    out.println(".TH " + doc.title + " " + doc.category.id +
                "  \"" + doc.date + "\" \"version " + doc.version +
                "\" \"" + doc.category + "\"")

    doc.sections foreach (s => emitSection(s, 1))
  }

  def main(args: Array[String]) = args match{
    case Array(classname)           => emitManPage(classname)
    case Array(classname, file, _*) => emitManPage(classname, new java.io.FileOutputStream(file))
    case _                          => sys.exit(1)
  }

  def emitManPage(classname: String, outStream: java.io.OutputStream = out.out) {
    if(outStream != out.out) out setOut outStream
    try {
      val cl = this.getClass.getClassLoader()
      val clasz = cl loadClass classname
      val meth = clasz getDeclaredMethod "manpage"
      val doc = meth.invoke(null).asInstanceOf[Document]
      emitDocument(doc)
    } catch {
      case ex: Exception =>
        ex.printStackTrace()
        System.err println "Error in EmitManPage"
        sys.exit(1)
    }
  }
}
