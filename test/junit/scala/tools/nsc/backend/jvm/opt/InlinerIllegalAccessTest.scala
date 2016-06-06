package scala.tools.nsc
package backend.jvm
package opt

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.collection.JavaConverters._
import scala.tools.asm.Opcodes._
import scala.tools.asm.tree._
import scala.tools.nsc.backend.jvm.AsmUtils._
import scala.tools.testing.BytecodeTesting

@RunWith(classOf[JUnit4])
class InlinerIllegalAccessTest extends BytecodeTesting {
  override def compilerArgs = "-opt:l:none"

  import compiler._
  import global.genBCode.bTypes._

  def addToRepo(cls: List[ClassNode]): Unit = for (c <- cls) byteCodeRepository.add(c, None)
  def assertEmpty(ins: Option[AbstractInsnNode]) = for (i <- ins)
    throw new AssertionError(textify(i))

  @Test
  def typeAccessible(): Unit = {
    val code =
      """package a {
        |  private class C {            // the Scala compiler makes all classes public
        |    def f1 = new C                   // NEW a/C
        |    def f2 = new Array[C](0)         // ANEWARRAY a/C
        |    def f3 = new Array[Array[C]](0)  // ANEWARRAY [La/C;
        |  }
        |  class D
        |}
        |package b {
        |  class E
        |}
      """.stripMargin

    val allClasses = compileClasses(code)
    val List(cClass, dClass, eClass) = allClasses
    assert(cClass.name == "a/C" && dClass.name == "a/D" && eClass.name == "b/E", s"${cClass.name}, ${dClass.name}, ${eClass.name}")
    addToRepo(allClasses) // they are not on the compiler's classpath, so we add them manually to the code repo

    val methods = cClass.methods.asScala.filter(_.name(0) == 'f').toList

    def check(classNode: ClassNode, test: Option[AbstractInsnNode] => Unit) = {
      for (m <- methods)
        test(inliner.findIllegalAccess(m.instructions, classBTypeFromParsedClassfile(cClass.name), classBTypeFromParsedClassfile(classNode.name)).map(_._1))
    }

    check(cClass, assertEmpty)
    check(dClass, assertEmpty)
    check(eClass, assertEmpty) // C is public, so accessible in E

    byteCodeRepository.parsedClasses.clear()
    classBTypeFromInternalName.clear()

    cClass.access &= ~ACC_PUBLIC // ftw
    addToRepo(allClasses)

    // private classes can be accessed from the same package
    check(cClass, assertEmpty)
    check(dClass, assertEmpty) // accessing a private class in the same package is OK
    check(eClass, {
      case Some(ti: TypeInsnNode) if Set("a/C", "[La/C;")(ti.desc) => ()
      // MatchError otherwise
    })
  }

  @Test
  def memberAccessible(): Unit = {
    val code =
      """package a {
        |  class C {
        |    /*public*/  def a = 0
        |    /*default*/ def b = 0
        |    protected   def c = 0
        |    private     def d = 0
        |
        |    /*public static*/    def e = 0
        |    /*default static*/   def f = 0
        |    protected /*static*/ def g = 0
        |    private /*static*/   def h = 0
        |
        |    def raC = a
        |    def rbC = b
        |    def rcC = c
        |    def rdC = d
        |    def reC = e
        |    def rfC = f
        |    def rgC = g
        |    def rhC = h
        |  }
        |
        |  class D extends C {
        |    def rbD = b // 1: default access b, accessed in D, declared in C. can be inlined into any class in the same package as C.
        |    def rcD = c // 2: protected c, accessed in D. can be inlined into C, D or E, but not into F (F and D are unrelated).
        |
        |    def rfD = f // 1
        |    def rgD = g // 2
        |  }
        |  class E extends D
        |
        |  class F extends C
        |
        |  class G
        |}
        |
        |package b {
        |  class H extends a.C
        |  class I
        |}
      """.stripMargin

    val allClasses = compileClasses(code)
    val List(cCl, dCl, eCl, fCl, gCl, hCl, iCl) = allClasses
    addToRepo(allClasses)

    // set flags that Scala scala doesn't (default access, static) - a hacky way to test all access modes.
    val names = ('a' to 'h').map(_.toString).toSet
    val List(a, b, c, d, e, f, g, h) = cCl.methods.asScala.toList.filter(m => names(m.name))

    def checkAccess(a: MethodNode, expected: Int): Unit = {
      assert((a.access & (ACC_STATIC | ACC_PUBLIC | ACC_PROTECTED | ACC_PRIVATE)) == expected, s"${a.name}, ${a.access}")
    }

                                                        checkAccess(a, ACC_PUBLIC)
    b.access &= ~ACC_PUBLIC;                            checkAccess(b, 0) // make it default access
    c.access &= ~ACC_PUBLIC; c.access |= ACC_PROTECTED; checkAccess(c, ACC_PROTECTED) // make it protected - scalac actually never emits PROTECTED in bytecode, see javaFlags in BTypesFromSymbols
                                                        checkAccess(d, ACC_PRIVATE)

                             e.access |= ACC_STATIC;                   checkAccess(e, ACC_STATIC | ACC_PUBLIC)
    f.access &= ~ACC_PUBLIC; f.access |= ACC_STATIC;                   checkAccess(f, ACC_STATIC)
    g.access &= ~ACC_PUBLIC; g.access |= (ACC_STATIC | ACC_PROTECTED); checkAccess(g, ACC_STATIC | ACC_PROTECTED)
                             h.access |= ACC_STATIC;                   checkAccess(h, ACC_STATIC | ACC_PRIVATE)

    val List(raC, rbC, rcC, rdC, reC, rfC, rgC, rhC) = cCl.methods.asScala.toList.filter(_.name(0) == 'r').sortBy(_.name)

    val List(rbD, rcD, rfD, rgD) = dCl.methods.asScala.toList.filter(_.name(0) == 'r').sortBy(_.name)

    def check(method: MethodNode, decl: ClassNode, dest: ClassNode, test: Option[AbstractInsnNode] => Unit): Unit = {
      test(inliner.findIllegalAccess(method.instructions, classBTypeFromParsedClassfile(decl.name), classBTypeFromParsedClassfile(dest.name)).map(_._1))
    }

    val cOrDOwner = (_: Option[AbstractInsnNode] @unchecked) match {
      case Some(mi: MethodInsnNode) if Set("a/C", "a/D")(mi.owner) => ()
      // MatchError otherwise
    }

    // PUBLIC

    // public methods allowed everywhere
    for (m <- Set(raC, reC); c <- allClasses) check(m, cCl, c, assertEmpty)

    // DEFAULT ACCESS

    // default access OK in same package
    for ((m, declCls) <- Set((rbC, cCl), (rfC, cCl), (rbD, dCl), (rfD, dCl)); c <- allClasses) {
      if (c.name startsWith "a/") check(m, declCls, c, assertEmpty)
      else check(m, declCls, c, cOrDOwner)
    }

    // PROTECTED

    // protected static accessed in same class, or protected static accessed in subclass(rgD).
    // can be inlined to sub- and superclasses, and classes in the same package (gCl)
    for ((m, declCls) <- Set((rgC, cCl), (rgD, dCl)); c <- Set(cCl, dCl, eCl, fCl, gCl, hCl)) check(m, declCls, c, assertEmpty)

    // protected in non-subclass and different package
    for (m <- Set(rcC, rgC)) check(m, cCl, iCl, cOrDOwner)

    // non-static protected accessed in subclass (rcD).
    // can be inlined only if the destination class is related (sub- or superclass) or in the same package,
    // AND if the receiver object is a subtype of the destination class
    // TODO: we cannot check this yet, so the check flags the instruction as causing an IllegalAccess. https://github.com/scala-opt/scala/issues/13
    for ((m, declCls) <- Set((rcC, cCl), (rcD, dCl)); c <- Set(cCl, dCl, eCl, fCl, gCl)) check(m, declCls, c, cOrDOwner)

    // rcD cannot be inlined into non-related classes, if the declaration and destination are not in the same package
    for (c <- Set(hCl, iCl)) check(rcD, dCl, c, cOrDOwner)

    // PRIVATE

    // privated method accesses can only be inlined in the same class
    for (m <- Set(rdC, rhC)) check(m, cCl, cCl, assertEmpty)
    for (m <- Set(rdC, rhC); c <- allClasses.tail) check(m, cCl, c, cOrDOwner)
  }
}
