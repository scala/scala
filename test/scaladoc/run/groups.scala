import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  override def code = """
      package test.scaladoc {

        /** @groupname Z From owner chain */
        package object `groups`

        package groups {
          /**
           * The trait A
           * @groupdesc A Group A is the group that contains functions starting with f
           * For example:
           * {{{
           *    this is an example
           * }}}
           * @groupdesc B Group B is the group that contains functions starting with b
           * @groupname B Group B has a nice new name and a high priority
           * @groupprio B -10
           * @group Traits
           * @note This is a note
           */
          trait A {
            /** foo description
             *  @group A */
            def foo = 1

            /** bar description
             *  @group B */
            def bar = 2
          }

          /** The trait B
           *  @group Traits
           *  @groupdesc C Group C is introduced by B
           */
          trait B {
            /** baz description
             *  @group C */
            def baz = 3
          }

          /** The class C which inherits from both A and B
           *  @group Classes
           *  @groupdesc B Look ma, I'm overriding group descriptions!!!
           *  @groupname B And names
           */
          class C extends A with B {
            /** Oh noes, I lost my group -- or did I?!? */
            override def baz = 4
          }
        }
      }
  """

  // no need for special settings
  def scaladocSettings = "-feature"

  def testModel(rootPackage: Package) = {
    // get the quick access implicit defs in scope (_package(s), _class(es), _trait(s), object(s) _method(s), _value(s))
    import access._

    // just need to check the member exists, access methods will throw an error if there's a problem
    val base = rootPackage._package("test")._package("scaladoc")._package("groups")

    def checkGroup(mbr: MemberEntity, grp: String) =
      assert(mbr.group == grp, "Incorrect group for " + mbr.qualifiedName + ": " + mbr.group + " instead of " + grp)

    def checkGroupDesc(dtpl: DocTemplateEntity, grp: String, grpDesc: String) = {
      assert(dtpl.groupDescription(grp).isDefined,
             "Group description for " + grp + " not defined in " + dtpl.qualifiedName)
      assert(extractCommentText(dtpl.groupDescription(grp).get).contains(grpDesc),
             "Group description for " + grp + " in " + dtpl.qualifiedName + " does not contain \"" + grpDesc + "\": \"" +
             extractCommentText(dtpl.groupDescription(grp).get) + "\"")
    }

    def checkGroupName(dtpl: DocTemplateEntity, grp: String, grpName: String) =
      // TODO: See why we need trim here, we already do trimming in the CommentFactory
      assert(dtpl.groupName(grp) == grpName,
             "Group name for " + grp + " in " + dtpl.qualifiedName + " does not equal \"" + grpName + "\": \"" + dtpl.groupName(grp) + "\"")

    def checkGroupPrio(dtpl: DocTemplateEntity, grp: String, grpPrio: Int) =
      assert(dtpl.groupPriority(grp) == grpPrio,
             "Group priority for " + grp + " in " + dtpl.qualifiedName + " does not equal " + grpPrio + ": " + dtpl.groupPriority(grp))


    val A = base._trait("A")
    val B = base._trait("B")
    val C = base._class("C")
    checkGroup(A, "Traits")
    checkGroup(B, "Traits")
    checkGroup(C, "Classes")
    checkGroup(A._method("foo"), "A")
    checkGroup(A._method("bar"), "B")
    checkGroup(B._method("baz"), "C")
    checkGroup(C._method("foo"), "A")
    checkGroup(C._method("bar"), "B")
    checkGroup(C._method("baz"), "C")

    checkGroupDesc(A, "A", "Group A is the group that contains functions starting with f")
    checkGroupName(A, "A", "A")
    checkGroupPrio(A, "A", 0)
    checkGroupDesc(A, "B", "Group B is the group that contains functions starting with b")
    checkGroupName(A, "B", "Group B has a nice new name and a high priority")
    checkGroupPrio(A, "B", -10)
    checkGroupName(A, "Z", "From owner chain")

    checkGroupDesc(B, "C", "Group C is introduced by B")
    checkGroupName(B, "C", "C")
    checkGroupPrio(B, "C", 0)
    checkGroupName(B, "Z", "From owner chain")

    checkGroupDesc(C, "A", "Group A is the group that contains functions starting with f")
    checkGroupName(C, "A", "A")
    checkGroupPrio(C, "A", 0)
    checkGroupDesc(C, "B", "Look ma, I'm overriding group descriptions!!!")
    checkGroupName(C, "B", "And names")
    checkGroupPrio(C, "B", -10)
    checkGroupDesc(C, "C", "Group C is introduced by B")
    checkGroupName(C, "C", "C")
    checkGroupPrio(C, "C", 0)
    checkGroupName(C, "Z", "From owner chain")
  }
}