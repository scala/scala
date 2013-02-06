import scala.tools.nsc.doc
import scala.tools.nsc.doc.base._
import scala.tools.nsc.doc.base.comment._
import scala.tools.nsc.interactive._
import scala.tools.nsc.interactive.tests._
import scala.tools.nsc.util._
import scala.tools.nsc.io._

object Test extends InteractiveTest {
  override val settings: doc.Settings = docSettings

  val tags = Seq(
    "@example  `\"abb\".permutations = Iterator(abb, bab, bba)`",
    "@version 1.0, 09/07/2012",
    "@since 2.10",
    "@todo this method is unsafe",
    "@note Don't inherit!",
    "@see some other method"
  )

  val comment = "This is a test comment."
  val caret = "<caret>"

  def text(nTags: Int) = 
    """|/** %s
       |
       | *  %s */
       |trait Commented {}
       |class User(c: %sCommented)""".stripMargin.format(comment, tags take nTags mkString "\n", caret)

  override lazy val compiler = {
    new {
      override val settings = {
        prepareSettings(Test.this.settings)
        Test.this.settings
      }
    } with Global(settings, compilerReporter) with MemberLookupBase with CommentFactoryBase {
      val global: this.type = this
      def chooseLink(links: List[LinkTo]): LinkTo = links.head
      def internalLink(sym: Symbol, site: Symbol) = None
      def toString(link: LinkTo) = link.toString

      def getComment(sym: Symbol, source: SourceFile) = {
        val docResponse = new Response[(String, String, Position)]
        askDocComment(sym, sym.owner, source, docResponse)
        docResponse.get.left.toOption flatMap {
          case (expanded, raw, pos) =>
            if (expanded.isEmpty)
              None
            else
              Some(ask { () => parseAtSymbol(expanded, raw, pos, Some(sym.owner)) })
        }
      }
    }
  }

  override def runDefaultTests() {
    for (i <- 1 to tags.length) {
      val markedText = text(i)
      val idx = markedText.indexOf(caret)
      val fileText = markedText.substring(0, idx) + markedText.substring(idx + caret.length)
      val source = sourceFiles(0) 
      val batch = new BatchSourceFile(source.file, fileText.toCharArray)
      val reloadResponse = new Response[Unit]
      compiler.askReload(List(batch), reloadResponse)
      reloadResponse.get.left.toOption match {
        case None =>
          reporter.println("Couldn't reload")
        case Some(_) =>
          val treeResponse = new compiler.Response[compiler.Tree]
          val pos = compiler.rangePos(batch, idx, idx, idx)
          compiler.askTypeAt(pos, treeResponse)
          treeResponse.get.left.toOption match {
            case Some(tree) =>
              val sym = tree.tpe.typeSymbol
              compiler.getComment(sym, batch) match {
                case None => println("Got no doc comment")
                case Some(comment) =>
                  import comment._
                  val tags: List[(String, Iterable[Body])] =
                    List(("@example", example), ("@version", version), ("@since", since.toList), ("@todo", todo), ("@note",  note), ("@see", see))
                  val str = ("body:" + body + "\n") + 
                    tags.map{ case (name, bodies) => name + ":" + bodies.mkString("\n") }.mkString("\n")
                  println(str)
              }
            case None => println("Couldn't find a typedTree")
          }
      }
    }
  }
}
