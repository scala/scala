/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
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
      out.println("<div>")
      emitText(text)
      out.println("\n</div>")
    }
    def emitText(text: AbstractText) {
      text match {
        case seq:SeqText =>
          seq.components.foreach(emitText)

        case seq:SeqPara =>
          seq.components.foreach(emitPara)

        case Text(text) =>
          out.print(escape(text))

        case BSlash =>
          out.print("\\")

        case MDash =>
          out.print("&#8212;")

        case NDash =>
          out.print("&#8211;")

        case Bold(text) =>
          out.print("<b>")
          emitText(text)
          out.print("</b>")

        case Italic(text) =>
          out.print("<i>")
          emitText(text)
          out.print("</i>")

        case Emph(text) =>
          out.print("<em>")
          emitText(text)
          out.print("</em>")

        case Mono(text) =>
          out.print("<code>")
          emitText(text)
          out.print("</code>")

        case Quote(text) =>
          out.print("\"")
          emitText(text)
          out.print("\"")

        case DefinitionList(definitions @ _*) =>
          out.println("<ins><dl>")
          for (d <- definitions) {
            out.println("<dt>")
            emitText(d.term)
            out.println("\n</dt>")
            out.println("<dd>")
            emitText(d.description)
            out.println("</dd>")
          }
          out.println("</dl></ins>")

        case Link(label, url) =>
           out.print("<a href=\"" + url + "\">")
           emitText(label)
           out.print("</a>")

        case _ =>
          error("unknown text node: " + text)
      }
    }

    def emitParagraph(para: Paragraph) {
      para match {
        case TextParagraph(text) =>
          out.println("<p>")
          emitText(text)
          out.println("</p>")

        case BlockQuote(text) =>
          out.println("<blockquote><p>")
          emitText(text)
          out.println("</p></blockquote>")

        case CodeSample(text) =>
          out.print("<pre>")
          out.print(escape(text))
          out.println("</pre>")

        case lst:BulletList =>
          out.println("<ul>")
          for (item <- lst.items) {
            out.print("<li>")
            emitText(item)
            out.println("</li>")
          }
          out.println("</ul>")

        case lst:NumberedList =>
          out.println("<ol>")
          for (item <- lst.items) {
            out.print("<li>")
            emitText(item)
          }
          out.println("</ol>")

        case TitledPara(title, text) =>
          out.println("<p><strong>" + escape(title) + "</strong></p>")
          emitText(text)

        case EmbeddedSection(sect) =>
          emitSection(sect, depth + 1)

        case _ =>
          error("unknown paragraph node: " + para)
      }
    }

    val name = section.title.replaceAll("\\p{Space}", "_").toLowerCase()
    out.println("\n<h" + depth + " id=\"" + name + "\">" +
                section.title +
                "</h" + depth + ">")
    section.paragraphs.foreach(emitParagraph)
  }

  private def emit3columns(col1: String, col2: String, col3: String) {
    out.println("<div style=\"float:left;\">")
    out.println(col1)
    out.println("</div>")
    out.println("<div style=\"float:right;\">")
    out.println(col3)
    out.println("</div>")
    out.println("<div style=\"text-align:center;\">")
    out.println(col2)
    out.println("</div>")
  }

  private def emitHeader(col1: String, col2: String, col3: String) {
    out.println("<!-- header -->")
    out.println("<div style=\"margin: 0 0 2em 0;\">")
    emit3columns(col1, col2, col3)
    out.println("</div>")
  }

  private def emitFooter(col1: String, col2: String, col3: String) {
    out.println("<!-- footer -->")
    out.println("<div style=\"margin: 2em 0 0 0;\">")
    emit3columns(col1, col2, col3)
    out.println("</div>")
  }

  def emitDocument(document: Document) {
    out.println("<?xml version=\"1.1\" encoding=\"" + document.encoding + "\"?>")
    out.println("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">")
    out.println("<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">\n")

    out.println("<head>")
    out.println("<title>" + document.title + " man page</title>")
    out.println("<meta http-equiv=\"Content-Language\" content=\"en\"/>")
    out.println("<meta http-equiv=\"Content-Type\" content=\"text/html; charset=" +
                  document.encoding + "\"/>")
    out.println("<meta name=\"Author\" content=\"" + document.author + "\"/>")
    out.println("<style type=\"text/css\">")
    out.println("  <!--")
    out.println("  blockquote, pre { margin:1em 4em 1em 4em; }")
    out.println("  dt { margin: 0.6em 0 0 0; }")
    out.println("  p { margin:0.6em 2em 0.6em 2em; text-align:justify; }")
    out.println("  //-->")
    out.println("</style>")
    out.println("</head>\n")

    out.println("<body>")
    val name = document.title + "(" + document.category.id + ")"
    emitHeader(name, "" + document.category, name)

    document.sections.foreach(s => emitSection(s, 3))

    emitFooter("version " + document.version, document.date, name)

    out.println("</body>")
    out.println("</html>")
  }
/* */
/*
 private def group(ns: Iterable[NodeSeq]): NodeSeq = {
    val zs = new NodeBuffer
    for (z <- ns) { zs &+ z }
    zs
  }

  def emitSection(section: Section, depth: int): NodeSeq = {
    def emitText(text: AbstractText): NodeSeq = text match {
      case seq:SeqText =>
        group(seq.components.toList.map(item => emitText(item)))

      case Text(text) =>
        scala.xml.Text(escape(text))

      case MDash =>
        scala.xml.Text("&#8212;")

      case NDash =>
        scala.xml.Text("&#8211;")

      case Bold(text) =>
        <b>{emitText(text)}</b>

      case Italic(text) =>
        <i>{emitText(text)}</i>

      case Emph(text) =>
        <em>{emitText(text)}</em>

      case Mono(text) =>
        <code>{emitText(text)}</code>

      case Quote(text) =>
        emitText("\"" & text & "\"")

      case DefinitionList(definitions @ _*) =>
        <ins><dl>
          {definitions.toList.map(d =>
            <dt>{emitText(d.term)}</dt>
            <dd>{emitText(d.description)}</dd>
          )}
        </dl></ins>

      case Link(label, url) =>
        <a href={url}>{emitText(label)}</a>

      case _ =>
        error("unknown text node " + text)
    }

    def emitParagraph(para: Paragraph): NodeSeq = para match {
      case TextParagraph(text) =>
        <p>{emitText(text)}</p>

      case BlockQuote(text) =>
        <blockquote>{emitText(text)}</blockquote>

      case CodeSample(text) =>
        <blockquote><pre>{escape(text)}</pre></blockquote>

      case lst:BulletList =>
        <ul>
          {lst.items.toList.map(item => <li>{emitText(item)}</li>)}
        </ul>

      case lst:NumberedList =>
        <ol>
          {lst.items.toList.map(item => <li>{emitText(item)}</li>)}
        </ol>

      case TitledPara(title, text) =>
        <p><strong>{escape(title)}</strong></p>
        {emitText(text)}

      case EmbeddedSection(sect) =>
        {emitSection(sect, depth + 1)}

      case _ =>
        error("unknown paragraph node " + para)
    }

    val name = section.title.replaceAll("\\p{Space}", "_").toLowerCase()
    <h3 id={name}>{section.title}</h3>.concat(
    group(section.paragraphs.toList.map(p => emitParagraph(p))))
  }

  private def emit3columns(col1: String, col2: String, col3: String): NodeSeq =
    <div style="float:left;">{col1}</div>
    <div style="float:right;">{col3}</div>
    <div style="text-align:center;">{col2}</div>
    <div style="clear:both;"></div>

  private def emitHeader(col1: String, col2: String, col3: String): NodeSeq =
    <div style="margin: 0 0 2em 0;">
      {emit3columns(col1, col2, col3)}
    </div>

  private def emitFooter(col1: String, col2: String, col3: String): NodeSeq = {
    scala.xml.Comment("footer")
    <div style="margin: 2em 0 0 0;">
      {emit3columns(col1, col2, col3)}
    </div>
  }

  def emitDocument(document: Document, addDocType: Boolean) = {
      val name = document.title + "(" + document.category.id + ")"
      val doc =
        <html xml:lang="en">
        <head>
          <title>{document.title}</title>
          <meta http-equiv="Content-Language" content="en"/>
          <meta http-equiv="Content-Type" content={"text/html; charset=" + document.encoding}/>
          <meta name="Author" content={document.author}/>
          <style type="text/css">
          {"  blockquote, pre { margin:1em 4em 1em 4em; }\n" +
           "  p { margin:1em 2em 1em 2em; text-align:justify; }\n"}
          </style>
        </head>
        <body>
          {emitHeader(name, "" + document.category, name)}
          {document.sections.map(s => emitSection(s, 2))}
          {emitFooter("version " + document.version, document.date, name)}
        </body>
      </html>
    out.println(doc)
/*
    val w = new java.io.StringWriter
    val id = scala.xml.dtd.PublicID("PUBLIC", null)
    val dtd = null //scala.xml.dtd.DEFAULT(true, "")
    val doctype = scala.xml.dtd.DocType("html", id, null) //List(dtd))
    XML.write(w, doc, document.encoding, true/ *xmlDecl* /, doctype)
    out.println(w.toString())
*/
  }
*/
  def main(args: Array[String]) {
    if (args.length < 1) {
      System.err.println("usage: EmitHtml <classname>")
      exit(1)
    }
    try {
      val cl = this.getClass.getClassLoader()
      val clasz = cl.loadClass(args(0))
      val meth = clasz.getDeclaredMethod("manpage")
      val doc = meth.invoke(null).asInstanceOf[Document]
      emitDocument(doc)
    } catch {
      case ex: Exception =>
        ex.printStackTrace()
        System.err.println("Error in EmitHtml")
        exit(1)
    }
  }

  def emitHtml(classname: String, outStream: java.io.OutputStream) {
    out.setOut(outStream)
    main(Array(classname))
  }
}
