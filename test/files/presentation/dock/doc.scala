//> using options -Xlint -Werror
import scala.reflect.internal.util.SourceFile
import scala.tools.nsc.doc.{ScaladocAnalyzer, ScaladocGlobalTrait}
import scala.tools.nsc.doc.base.{CommentFactoryBase, LinkTo, MemberLookupBase}
import scala.tools.nsc.doc.base.comment.Comment
import scala.tools.nsc.interactive.{/*CommentPreservingTypers,*/ Global, InteractiveAnalyzer}
import scala.tools.nsc.interactive.tests.{InteractiveTest}

object Test extends InteractiveTest {
  //override protected def argsString: String = "-Ypresentation-debug -Ypresentation-verbose"

  type TestGlobal = Global {
    def getComment(sym: Symbol, source: SourceFile, fragments: List[(Symbol, SourceFile)]): Option[Comment]
  }

  override lazy val compiler: TestGlobal = {
    prepareSettings(settings)
    new Global(settings, compilerReporter, "doctest") with MemberLookupBase with CommentFactoryBase with ScaladocGlobalTrait {
      outer =>

      val global: this.type = this

      @annotation.nowarn
      override lazy val analyzer = new {
        val global: outer.type = outer
      } with ScaladocAnalyzer with InteractiveAnalyzer /*with CommentPreservingTypers*/ {
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
        docResponse.get.swap.toOption.flatMap {
          case (expanded, raw, pos) if !expanded.isEmpty =>
            Some(ask { () => parseAtSymbol(expanded, raw, pos, sym.owner) })
          case _ => None
        }
      }
    }
  }

  override def runDefaultTests(): Unit = dorun()

  def dorun(): Boolean = {
    import compiler.{TermName, TypeName, getComment}
    def findSource(name: String) = sourceFiles.find(_.file.name == name).get

    // Check inter-classes documentation one-time retrieved ok.
    val baseSource = findSource("Base.scala")
    val derivedSource = findSource("Derived.scala")
    def existsText(where: Any, text: String): Boolean = where match {
      case s: String => s.contains(text)
      case s: Seq[_] => s.exists(existsText(_, text))
      case p: Product => p.productIterator.exists(existsText(_, text))
      case c: Comment => existsText(c.body, text)
      case x          => throw new MatchError(x)
    }
    val (derived, base) = compiler.ask { () =>
      val derived = compiler.rootMirror.RootPackage.info.decl(TermName("p")).info.decl(TypeName("Derived"))
      (derived, derived.ancestors(0))
    }
    val cmt1 = getComment(derived, derivedSource, (base, baseSource) :: (derived, derivedSource) :: Nil)
    if (!existsText(cmt1, "This is Derived comment")) {
      println(s"Unexpected Derived class comment: $cmt1")
      return false
    }

    val (fooDerived, fooBase) = compiler.ask { () =>
      val decl = derived.tpe.decl(TermName("foo"))
      (decl, decl.allOverriddenSymbols.head)
    }

    getComment(fooDerived, derivedSource, (fooBase, baseSource) :: (fooDerived, derivedSource) :: Nil) match {
      case Some(cmt2) =>
        if (!existsText(cmt2, "Base method has documentation")) {
          println(s"Unexpected foo method ($fooDerived) comment: $cmt2")
          return false
        }
      case None =>
        println(s"Missing foo method ($fooDerived) comment")
        return false
    }
    true
  }
}
