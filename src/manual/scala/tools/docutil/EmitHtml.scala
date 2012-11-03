/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Stephane Micheloud
 * Adapted from Lex Spoon's sbaz manual
 */

package scala.tools.docutil

object EmitHtml {
  import scala.xml.{Node, NodeBuffer, NodeSeq, XML}
  import ManPage._

  val out = Console

  def escape(text: String) =
    text.replaceAll("&", "&amp;")
        .replaceAll("<", "&lt;")
        .replaceAll(">", "&gt;")

/* */
  def emitSection(section: Section, depth: Int) {
    def emitPara(text: AbstractText) {
      out println "<div>"
      emitText(text)
      out println "\n</div>"
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
          out print "\\"

        case MDash =>
          out print "&#8212;"

        case NDash =>
          out print "&#8211;"

        case Bold(text) =>
          out print "<b>"
          emitText(text)
          out print "</b>"

        case Italic(text) =>
          out print "<i>"
          emitText(text)
          out print "</i>"

        case Emph(text) =>
          out print "<em>"
          emitText(text)
          out print "</em>"

        case Mono(text) =>
          out print "<code>"
          emitText(text)
          out print "</code>"

        case Quote(text) =>
          out print "\""
          emitText(text)
          out print "\""

        case DefinitionList(definitions @ _*) =>
          out println "<ins><dl>"
          for (d <- definitions) {
            out println "<dt>"
            emitText(d.term)
            out println "\n</dt>"
            out println "<dd>"
            emitText(d.description)
            out println "</dd>"
          }
          out println "</dl></ins>"

        case Link(label, url) =>
           out.print("<a href=\"" + url + "\">")
           emitText(label)
           out print "</a>"

        case _ =>
          sys.error("unknown text node: " + text)
      }
    }

    def emitParagraph(para: Paragraph) {
      para match {
        case TextParagraph(text) =>
          out println "<p>"
          emitText(text)
          out println "</p>"

        case BlockQuote(text) =>
          out println "<blockquote><p>"
          emitText(text)
          out println "</p></blockquote>"

        case CodeSample(text) =>
          out print "<pre>"
          out print escape(text)
          out println "</pre>"

        case lst:BulletList =>
          out println "<ul>"
          for (item <- lst.items) {
            out print "<li>"
            emitText(item)
            out println "</li>"
          }
          out println "</ul>"

        case lst:NumberedList =>
          out println "<ol>"
          for (item <- lst.items) {
            out print "<li>"
            emitText(item)
          }
          out println "</ol>"

        case TitledPara(title, text) =>
          out.println("<p><strong>" + escape(title) + "</strong></p>")
          emitText(text)

        case EmbeddedSection(sect) =>
          emitSection(sect, depth + 1)

        case _ =>
          sys.error("unknown paragraph node: " + para)
      }
    }

    val name = section.title.replaceAll("\\p{Space}", "_").toLowerCase()
    out.println("\n<h" + depth + " id=\"" + name + "\">" +
                section.title +
                "</h" + depth + ">")
    section.paragraphs foreach emitParagraph
  }

  private def emit3columns(col1: String, col2: String, col3: String) {
    out println "<div style=\"float:left;\">"
    out println col1
    out println "</div>"
    out println "<div style=\"float:right;\">"
    out println col3
    out println "</div>"
    out println "<div style=\"text-align:center;\">"
    out println col2
    out println "</div>"
  }

  private def emitHeader(col1: String, col2: String, col3: String) {
    out println "<!-- header -->"
    out println "<div style=\"margin: 0 0 2em 0;\">"
    emit3columns(col1, col2, col3)
    out println "</div>"
  }

  private def emitFooter(col1: String, col2: String, col3: String) {
    out println "<!-- footer -->"
    out println "<div style=\"margin: 2em 0 0 0;\">"
    emit3columns(col1, col2, col3)
    out println "</div>"
  }

  def emitDocument(document: Document) {
    out.println("<?xml version=\"1.1\" encoding=\"" + document.encoding + "\"?>")
    out.println("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">")
    out.println("<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">\n")

    out println "<head>"
    out.println("<title>" + document.title + " man page</title>")
    out.println("<meta http-equiv=\"Content-Language\" content=\"en\"/>")
    out.println("<meta http-equiv=\"Content-Type\" content=\"text/html; charset=" +
                  document.encoding + "\"/>")
    out.println("<meta name=\"Author\" content=\"" + document.author + "\"/>")
    out println "<style type=\"text/css\">"
    out println "  <!--"
    out println "  blockquote, pre { margin:1em 4em 1em 4em; }"
    out println "  dt { margin: 0.6em 0 0 0; }"
    out println "  p { margin:0.6em 2em 0.6em 2em; text-align:justify; }"
    out println "  //-->"
    out println "</style>"
    out println "</head>\n"

    out println "<body>"
    val name = document.title + "(" + document.category.id + ")"
    emitHeader(name, "" + document.category, name)

    document.sections foreach (s => emitSection(s, 3))

    emitFooter("version " + document.version, document.date, name)

    out println "</body>"
    out println "</html>"
  }

  def main(args: Array[String]) = args match{
    case Array(classname)           => emitHtml(classname)
    case Array(classname, file, _*) => emitHtml(classname, new java.io.FileOutputStream(file))
    case _                          => sys.exit(1)
  }

  def emitHtml(classname: String, outStream: java.io.OutputStream = out.out) {
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
