import scala.tools.nsc.doc.base._
import scala.tools.nsc.doc.model._
import scala.tools.nsc.doc.model.diagram._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  override def code = """
        package scala.test.scaladoc.SI5235 {
          trait Builder[From, To]

          /**
           * @define Coll `GenericColl`
           */
          class GenericColl {
            /**
             * @usecase def reverse(): $Coll
             * Returns the reversed $Coll.
             */
            def reverse[T](implicit something: Builder[GenericColl, T]): T
            def foo1: GenericColl = ???
          }

          /** Nooo, don't point to this */
          trait MyCollection

          package specific {
            /**
             * @define Coll `BullSh`
             */
            trait SpecificColl extends GenericColl {
              def foo2: SpecificColl = ???
            }
          }

          package mycoll {
            /**
             * @define Coll `mycoll.MyCollection`
             */
            class MyCollection extends specific.SpecificColl {
              def foo3: MyCollection = ???
            }
          }
        }
    """

  // diagrams must be started. In case there's an error with dot, it should not report anything
  def scaladocSettings = ""

  def testModel(rootPackage: Package) = {
    // get the quick access implicit defs in scope (_package(s), _class(es), _trait(s), object(s) _method(s), _value(s))
    import access._

    val base = rootPackage._package("scala")._package("test")._package("scaladoc")._package("SI5235")

    val GenericColl = base._class("GenericColl")
    val SpecificColl = base._package("specific")._trait("SpecificColl")
    val MyCollection = base._package("mycoll")._class("MyCollection")

    // check comment text
    val gcComment = extractCommentText(GenericColl._method("reverse").comment.get)
    val scComment = extractCommentText(SpecificColl._method("reverse").comment.get)
    val mcComment = extractCommentText(MyCollection._method("reverse").comment.get)
    assert(gcComment.contains("Returns the reversed GenericColl."),
           gcComment + ".contains(\"Returns the reversed GenericColl.\")")
    assert(scComment.contains("Returns the reversed BullSh."),
           scComment + ".contains(\"Returns the reversed BullSh.\")")
    assert(mcComment.contains("Returns the reversed mycoll.MyCollection."),
           mcComment + ".contains(\"Returns the reversed mycoll.MyCollection.\")")

    // check signatures
    val gcReverse = GenericColl._method("reverse")
    val scReverse = SpecificColl._method("reverse")
    val mcReverse = MyCollection._method("reverse")
    val gcReverseType = gcReverse.resultType
    val scReverseType = scReverse.resultType
    val mcReverseType = mcReverse.resultType
    assert(gcReverseType.name == "GenericColl", gcReverseType.name + " == GenericColl")
    assert(scReverseType.name == "BullSh",      scReverseType.name + " == BullSh")
    assert(mcReverseType.name == "MyCollection",mcReverseType.name + " == MyCollection")
    assert(gcReverseType.refEntity(0)._1 == LinkToTpl(GenericColl),
           gcReverse.qualifiedName + "'s return type has a link to " + GenericColl.qualifiedName)
    assert(scReverseType.refEntity(0)._1 == Tooltip("BullSh"),
           scReverseType.refEntity(0)._1 + " == Tooltip(\"BullSh\")")
    assert(mcReverseType.refEntity(0)._1 == LinkToTpl(MyCollection),
           mcReverse.qualifiedName + "'s return type has a link to " + MyCollection.qualifiedName)
  }
}
