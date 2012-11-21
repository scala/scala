/* NSC -- new Scala compiler
 * Copyright 2007-2013 LAMP/EPFL
 * @author  David Bernard, Manohar Jonnalagedda
 */

package scala.tools.nsc
package doc
package html
package page

import model._
import scala.xml.{NodeSeq, Unparsed}
import java.io.File

class Source(sourceFile: File) extends HtmlPage {

  val path = List("source.html")

  val title = "Scaladoc: page source"

  val headers =
    NodeSeq.Empty

  val body =
    <body>
      <h1>Page source is not implemented yet</h1>
    </body>

    /*


    def readTextFromSrcDir(subPath: String) :Option[String] = {
      readTextFromFile(new File(sourceDir, subPath))
    }

    def readTextFromFile(f : File) :Option[String] = {
      if (f.exists) {
        Some(Source.fromFile(f)(Codec.default).getLines().mkString(""))
      } else {
        None
      }
    }


    def writeTextToFile(f : File, txt : String, header: Option[String], footer: Option[String]) {
      val out = new FileOutputStream(f)
      try {
        val enc = "UTF-8"
        header.foreach(s => out.write(s.getBytes(enc)))
        out.write(txt.getBytes(enc))
        footer.foreach(s => out.write(s.getBytes(enc)))
      } finally {
        try {
          out.close()
        } catch {
          case _ => //ignore
        }
      }
    }

    trait SourceHtmlizer {
      def scalaToHtml(src :File) : Option[File]
    }

    lazy val sourceHtmlizer : SourceHtmlizer = {
      if (cfg.htmlizeSource) {
        new SourceHtmlizer {

          val inDir: File = cfg.sourcedir
          val outDir: File = cfg.outputdir

          private def relativize(uri: URI, from: URI) = linkHelper.relativize(uri, from).getOrElse("__notFound__" + uri.getPath)

          def header(dest: URI) = Some("""
          <html>
          <head>
            <link href='""" + relativize(new URI("site:/_highlighter/SyntaxHighlighter.css"), dest) + """' rel='stylesheet' type='text/css'/>
            <script language='javascript' src='""" + relativize(new URI("site:/_highlighter/shAll.js"), dest) + """'></script>
          </head>
          <body>
            <pre name="code" class="scala" style="width:100%">
        """)

          def footer(dest: URI) = Some("""</pre>
            <script language='javascript'>
              dp.SyntaxHighlighter.ClipboardSwf = '""" + relativize(new URI("site:/_highlighter/clipboard.swf"), dest) + """';
              dp.SyntaxHighlighter.HighlightAll('code');
            </script>
          </body>
          </html>
        """)

          //TODO: escape the source code
          def scalaToHtml(src :File) = {
            val dest = new File(outDir, fileHelper.relativePathUnderDir(src, inDir) + ".html")
            if (!dest.exists || dest.lastModified < src.lastModified) {

              //we need to verify whether the directory we are trying to write to has already been created or not
              if(!dest.getParentFile.exists) dest.getParentFile.mkdirs

              val uri = linkHelper.uriFor(dest).get
              var txt = fileHelper.readTextFromFile(src).getOrElse("")
              txt = txt.replace("<", "&lt;")
              fileHelper.writeTextToFile(dest, txt, header(uri), footer(uri))
            }
            Some(dest)
          }

          def copyResources() {
            val loader = this.getClass().getClassLoader()
            val buf = new Array[Byte](1024)
            def copyResource(name: String) = fileHelper.copyResource("/scala/tools/nsc/doc/html/resource/", name, outDir, loader, buf)
            copyResource("_highlighter/clipboard.swf")
            copyResource("_highlighter/shAll.js")
            copyResource("_highlighter/SyntaxHighlighter.css")
          }

          copyResources()
        }
      } else {
        new SourceHtmlizer {
          def scalaToHtml(src :File) = None
        }
      }
    }
    */

}
