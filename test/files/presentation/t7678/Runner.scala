import scala.tools.nsc.interactive.tests._
import scala.reflect.internal.util._

object Test extends InteractiveTest {

  import compiler._, definitions._

  override def runDefaultTests() {
    def resolveTypeTagHyperlink() {
      val sym = compiler.askForResponse(() => compiler.currentRun.runDefinitions.TypeTagClass).get.swap.getOrElse(???)
      val r = new Response[Position]
      compiler.askLinkPos(sym, new BatchSourceFile("", source), r)
      r.get
    }

    def checkTypeTagSymbolConsistent() {
      compiler.askForResponse {
        () => {
          val runDefinitions = currentRun.runDefinitions
          import runDefinitions._
          assert(TypeTagsClass.map(sym => getMemberClass(sym, tpnme.TypeTag)) == TypeTagClass)
          assert(TypeTagsClass.map(sym => getMemberClass(sym, tpnme.WeakTypeTag)) == WeakTypeTagClass)
          assert(TypeTagsClass.map(sym => getMemberModule(sym, nme.WeakTypeTag)) == WeakTypeTagModule)
          assert(getMemberMethod(ReflectPackage, nme.materializeClassTag) == materializeClassTag)
          assert(ReflectApiPackage.map(sym => getMemberMethod(sym, nme.materializeWeakTypeTag)) == materializeWeakTypeTag)
          assert(ReflectApiPackage.map(sym => getMemberMethod(sym, nme.materializeTypeTag)) == materializeTypeTag)
          ()
        }
      }.get match {
        case Right(t) => t.printStackTrace
        case Left(_) =>
      }
    }
    resolveTypeTagHyperlink()
    // The presentation compiler loads TypeTags from source; we'll get new symbols for its members.
    // Need to make sure we didn't cache the old ones in Definitions.
    checkTypeTagSymbolConsistent()
  }

  def source =
    """
      |package scala
      |package reflect
      |package api
      |
      |trait TypeTags { self: Universe =>
      |  import definitions._
      |
      |  @annotation.implicitNotFound(msg = "No WeakTypeTag available for ${T}")
      |  trait WeakTypeTag[T] extends Equals with Serializable {
      |    val mirror: Mirror
      |    def in[U <: Universe with Singleton](otherMirror: scala.reflect.api.Mirror[U]): U # WeakTypeTag[T]
      |    def tpe: Type
      |  }
      |  object WeakTypeTag
      |
      |  trait TypeTag[T] extends WeakTypeTag[T] with Equals with Serializable {
      |  }
      |  object TypeTag
      |
    """.stripMargin
}
