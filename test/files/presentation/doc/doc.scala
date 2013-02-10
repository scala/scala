import scala.tools.nsc.doc
import scala.tools.nsc.doc.base._
import scala.tools.nsc.doc.base.comment._
import scala.tools.nsc.interactive._
import scala.tools.nsc.interactive.tests._
import scala.tools.nsc.util._

object Test extends InteractiveTest {
  override val settings: doc.Settings = docSettings

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

  lazy override val compiler = new {
      override val settings = {
        val s = Test.this.settings
        prepareSettings(s)
        s
      }
    } with Global(settings, compilerReporter) with CommentFactoryBase with MemberLookupBase {
   def chooseLink(links: List[LinkTo]): LinkTo = links.head
   def toString(link: LinkTo) = link.toString
   def internalLink(sym: Symbol, site: Symbol) = None
   override val global: this.type = this

    def getComment(sym: Symbol, source: SourceFile): Option[Comment] = {
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

  override def runDefaultTests() {
    import compiler._
    val clazz = compiler.ask { () => definitions.EmptyPackage.info.decl(newTypeName(names.head)) }
    for (name <- names;
         i <- 1 to tags.length) {
      val newText = text(name, i)
      val source = sourceFiles(0) 
      val batch = new BatchSourceFile(source.file, newText.toCharArray)
      val reloadResponse = new Response[Unit]
      compiler.askReload(List(batch), reloadResponse)
      reloadResponse.get.left.toOption match {
        case None =>
          println("Couldn't reload")
        case Some(_) =>
          val parseResponse = new Response[Tree]
          askParsedEntered(batch, true, parseResponse)
          parseResponse.get.left.toOption match {
            case None =>
              println("Couldn't parse")
            case Some(_) =>
              val sym = compiler.ask { () =>
                val toplevel = definitions.EmptyPackage.info.decl(newTypeName(name))
                if (toplevel eq NoSymbol) {
                  val term = clazz.info.decl(newTermName(name))
                  if (term eq NoSymbol) clazz.info.decl(newTypeName(name)) else
                    if (term.isAccessor) term.accessed else term
                } else toplevel
              }

              getComment(sym, batch) match {
                case None => println(s"Got no doc comment for $name")
                case Some(comment) =>
                  import comment._
                  def cnt(bodies: Iterable[Body]) = bodies.size
                  val actual = cnt(example) + cnt(version) + cnt(since) + cnt(todo) + cnt(note) + cnt(see)
                  if (actual != i)
                    println(s"Got docComment with $actual tags instead of $i, file text:\n$newText")
              }
          }
      }
    }
  }
}
