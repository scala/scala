import scala.tools.nsc.doc
import scala.tools.nsc.doc.base._
import scala.tools.nsc.doc.base.comment._
import scala.tools.nsc.interactive._
import scala.tools.nsc.interactive.tests._
import scala.tools.nsc.util._

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



  override lazy val compiler = {
    prepareSettings(settings)
    new Global(settings, compilerReporter) with MemberLookupBase with CommentFactoryBase {
      val global: this.type = this
      def chooseLink(links: List[LinkTo]): LinkTo = links.head
      def internalLink(sym: Symbol, site: Symbol) = None
      def toString(link: LinkTo) = link.toString
      def warnNoLink = false
      def findExternalLink(sym: Symbol, name: String) = None

      override def forScaladoc = true

      def getComment(sym: Symbol, source: SourceFile, overridden: List[(Symbol,SourceFile)]): Option[Comment] = {
        val docResponse = new Response[(String, String, Position)]
        askDocComment(sym, source, sym.owner, overridden, docResponse)
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
    import compiler._
    def findSource(name: String) = sourceFiles.find(_.file.name == name).get

    val className = names.head
    for (name <- names;
         i <- 1 to tags.length) {
      val newText = text(name, i)
      val source = findSource("Class.scala")
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
                  val clazz = definitions.EmptyPackage.info.decl(newTypeName(className))

                  val term = clazz.info.decl(newTermName(name))
                  if (term eq NoSymbol) clazz.info.decl(newTypeName(name)) else
                    if (term.isAccessor) term.accessed else term
                } else toplevel
              }

              getComment(sym, batch, List()) match {
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

    // Check inter-classes documentation one-time retrieved ok.
    val baseSource = findSource("Base.scala")
    val derivedSource = findSource("Derived.scala")
    def existsText(where: Any, text: String): Boolean = where match {
      case `text` => true
      case s: Seq[_] => s exists (existsText(_, text))
      case p: Product => p.productIterator exists (existsText(_, text))
    }
    val (derived, base) = compiler.ask { () => 
      val derived = definitions.RootPackage.info.decl(newTermName("p")).info.decl(newTypeName("Derived"))
      (derived, derived.ancestors(0))
    }
    val cmt1 = getComment(derived, derivedSource, (base, baseSource)::Nil)
    if (!existsText(cmt1, "Derived comment."))
      println("Unexpected Derived class comment:"+cmt1)

    val (fooDerived, fooBase) = compiler.ask { () =>
      val decl = derived.tpe.decl(newTermName("foo"))
      (decl, decl.allOverriddenSymbols(0))
    }

    val cmt2 = getComment(fooDerived, derivedSource, (fooBase, baseSource)::Nil)
    if (!existsText(cmt2, "Base method has documentation."))
      println("Unexpected foo method comment:"+cmt2)
  }
}
