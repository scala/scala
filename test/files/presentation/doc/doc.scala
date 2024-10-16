//> using options -Xlint -Werror
import scala.reflect.internal.util.{BatchSourceFile, SourceFile}
import scala.tools.nsc.doc.{ScaladocAnalyzer, ScaladocGlobalTrait}
import scala.tools.nsc.doc.base.{CommentFactoryBase, LinkTo, MemberLookupBase}
import scala.tools.nsc.doc.base.comment.{Body, Comment}
import scala.tools.nsc.interactive._
import scala.tools.nsc.interactive.tests._

object Test extends InteractiveTest {
  val tags = Seq(
    "@example  `\"abb\".permutations = Iterator(abb, bab, bba)`",
    "@version 1.0, 09/07/2012",
    "@since 2.10",
    "@todo this is unsafe!",
    "@note Don't inherit!",
    "@see something else"
  )

  val names = Seq("Class", "Def", "Val", "Var", "AbstracType", "TypeAlias", "Trait", "InnerClass")
  val bareText =
    """abstract class %s {
    |  def %s = ""
    |  val %s = ""
    |  var %s: String = _
    |  type %s
    |  type %s = String
    |  class %s
    |}
    |trait %s""".stripMargin.format(names: _*)

  def docComment(nTags: Int) = "/**\n%s*/".format(tags.take(nTags).mkString("\n"))

  def text(name: String, nTags: Int) = {
    val nameIndex = bareText.indexOf(name)
    val (pre, post) = bareText.splitAt(nameIndex)
    val crIndex = pre.lastIndexOf("\n")
    val (prepre, prepost) = pre.splitAt(crIndex)
    prepre + docComment(nTags) + prepost + post
  }

  override lazy val compiler: Global { def getComment(sym: Symbol, source: SourceFile, fragments: List[(Symbol, SourceFile)]): Option[Comment] } = {
    prepareSettings(settings)
    new Global(settings, compilerReporter) with MemberLookupBase with CommentFactoryBase with ScaladocGlobalTrait {
      outer =>

      val global: this.type = this

      @annotation.nowarn
      override lazy val analyzer = new {
        val global: outer.type = outer
      } with ScaladocAnalyzer with InteractiveAnalyzer {
        override def newTyper(context: Context): InteractiveTyper with ScaladocTyper =
          new Typer(context) with InteractiveTyper with ScaladocTyper
      }

      def chooseLink(links: List[LinkTo]): LinkTo = links.head
      def internalLink(sym: Symbol, site: Symbol) = None
      def toString(link: LinkTo) = link.toString
      def warnNoLink = false
      def findExternalLink(sym: Symbol, name: String) = None

      def getComment(sym: Symbol, source: SourceFile, fragments: List[(Symbol, SourceFile)]): Option[Comment] = {
        val docResponse = new Response[(String, String, Position)]
        askDocComment(sym, source, sym.owner, fragments, docResponse)
        docResponse.get.swap.toOption flatMap {
          case (expanded, raw, pos) =>
            if (expanded.isEmpty)
              None
            else
              Some(ask { () => parseAtSymbol(expanded, raw, pos, sym.owner) })
        }
      }
    }
  }

  override def runDefaultTests(): Unit = {
    import compiler.{NoSymbol, TermName, Tree, TypeName, askParsedEntered, getComment}
    def findSource(name: String) = sourceFiles.find(_.file.name == name).get

    val className = names.head
    for (name <- names; i <- 1 to tags.length) {
      val newText = text(name, i)
      val source = findSource("Class.scala")
      val batch = new BatchSourceFile(source.file, newText.toCharArray)
      val reloadResponse = new Response[Unit]
      compiler.askReload(List(batch), reloadResponse)
      reloadResponse.get.swap.toOption match {
        case None =>
          println("Couldn't reload")
        case Some(_) =>
          val parseResponse = new Response[Tree]
          askParsedEntered(batch, keepLoaded = true, parseResponse)
          parseResponse.get.swap.toOption match {
            case None =>
              println("Couldn't parse")
            case Some(_) =>
              val sym = compiler.ask { () =>
                val toplevel = compiler.rootMirror.EmptyPackage.info.decl(TypeName(name))
                if (toplevel eq NoSymbol) {
                  val clazz = compiler.rootMirror.EmptyPackage.info.decl(TypeName(className))
                  val term = clazz.info.decl(TermName(name))
                  if (term eq NoSymbol) clazz.info.decl(TypeName(name))
                  else if (term.isAccessor) term.accessed else term
                }
                else toplevel
              }

              getComment(sym, batch, sym -> batch :: Nil) match {
                case None => println(s"Got no doc comment for $name")
                case Some(comment) =>
                  import comment._
                  def cnt(bodies: Iterable[Body]) = bodies.size
                  val actual = cnt(example) + cnt(version.toList) + cnt(since.toList) + cnt(todo) + cnt(note) + cnt(see)
                  if (actual != i)
                    println(s"Got docComment with $actual tags instead of $i, file text:\n$newText")
              }
          }
      }
    }
  }
}
