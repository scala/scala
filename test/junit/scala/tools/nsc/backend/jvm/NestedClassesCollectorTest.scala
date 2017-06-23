package scala.tools.nsc.backend.jvm

import org.junit.{Ignore, Test}
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Assert._

import scala.tools.asm.tree.ClassNode
import scala.tools.nsc.backend.jvm.BTypes.InternalName
import scala.tools.nsc.backend.jvm.analysis.BackendUtils.NestedClassesCollector

class Collector extends NestedClassesCollector[String] {
  override def declaredNestedClasses(internalName: InternalName): List[String] = Nil
  override def getClassIfNested(internalName: InternalName): Option[String] = Some(internalName)
  def raiseError(msg: String, sig: String, e: Option[Throwable]): Unit =
    throw e.getOrElse(new Exception(msg + " " + sig))
}

@RunWith(classOf[JUnit4])
class NestedClassesCollectorTest {
  val c = new Collector
  def inners: List[String] = {
    val res = c.innerClasses.toList.sorted
    c.innerClasses.clear()
    res
  }

  @Test
  def referenceTypeSignatures(): Unit = {
    def ref(sig: String, expect: List[String]) = {
      c.visitFieldSignature(sig)
      assertEquals(inners, expect)
    }

    // TypeVariableSignature
    ref("THello;", Nil)
    ref("TT;TU;", Nil)
    ref("TT;TU;", Nil)

    ref("LKlass;", List("Klass"))
    ref("Lscala/pack/Kl;", List("scala/pack/Kl"))
    ref("Lscala/pack/Kl;", List("scala/pack/Kl"))
    ref("LA.B;", List("A", "A$B"))
    ref("Lp/Kl.Ne.In;", List("p/Kl", "p/Kl$Ne", "p/Kl$Ne$In"))
    ref("LA<*>;", List("A"))
    ref("LA<**+[I[JTFoo;-TBar;LB;*>;", List("A", "B"))
    ref("Lp/A<[I[LTBoo<*>;-[JTFoo;-TBar;Lp/B<[J+[Lp/C;>.N<+TT;*Lp/D;>;*>;", List("TBoo", "p/A", "p/B", "p/B$N", "p/C", "p/D"))
    ref("Lp/A<[I[Lp/B<*>;>;", List("p/A", "p/B"))
    ref("Lp/A<Lp/B;>.C<Lp/D;>.E;", List("p/A", "p/A$C", "p/A$C$E", "p/B", "p/D"))

    ref("[I", Nil)
    ref("[[[LA;", List("A"))
    ref("[[[LA<**+[I-[LB;>;", List("A", "B"))
  }

  @Test
  def classSignatures(): Unit = {
    def cls(sig: String, expect: List[String]) = {
      c.visitClassSignature(sig)
      assertEquals(inners, expect)
    }

    cls("LA;", List("A"))
    cls("LA;LB;", List("A", "B"))
    cls("Lp/a/A;Lp/B;", List("p/B", "p/a/A"))
    cls("<T:>LA;", List("A"))
    cls("<T:LA;:[I:TU;:[TV;>LB;", List("A", "B"))
    cls("<T:LA;:[Lp/B<+Lp/C;>;:TU;:[TV;>LC;", List("A", "C", "p/B", "p/C"))
    cls("<T::TT;>LA;", List("A"))
    cls("<T:LA;>LB;", List("A", "B")) // one type parameter T with class bound A
    cls("<P:TT;>LA;", List("A")) // one type parameter

    // Missing ClassBound without an interface bound. Probably the grammar only allows those by
    // accident. Our parser doesn't. https://stackoverflow.com/q/44284928
    // cls("<T:U:>LA;", List("A"))
    // cls("<T:L:>LA;", List("A"))
    // cls("<T:LA:>LB;", List("B")) // two type parameters, T and LA
    // cls("<P:TT:>LA;", List("A")) // two type parameters
    // cls("<T:U:LA;>LB;", List("A", "B"))
  }

  @Test
  def methodSignatures(): Unit = {
    def met(sig: String, expect: List[String]) = {
      c.visitMethodSignature(sig)
      assertEquals(inners, expect)
    }

    // type parameters implementation is the same as for class signatures, so only basic testing here
    met("()V", Nil)
    met("(BJI)Z", Nil)
    met("(IJLp/A;Z)Lp/B;", List("p/A", "p/B"))
    met("<X:LA;:[I:TU;:[TV;Y:[I:LB<+LC;>;>([I[[[LD<**>;)TT;", List("A", "B", "C", "D"))
    met("(LA;ITT;)I^LB;", List("A", "B"))
    met("()I^TT;^Lp/A<**+[[Lp/B;>;^TBA;", List("p/A", "p/B"))
    met("()V^TT;", Nil)
  }

  @Test
  @Ignore("manually run test")
  def rtJar(): Unit = {
    import java.nio.file._
    import scala.collection.JavaConverters._
    val zipfile = Paths.get("/Library/Java/JavaVirtualMachines/jdk1.8.0_131.jdk/Contents/Home/jre/lib/rt.jar")
    val fs = FileSystems.newFileSystem(zipfile, null)
    val root = fs.getRootDirectories.iterator().next()
    val contents = Files.walk(root).iterator().asScala.toList
    for (f <- contents if Files.isRegularFile(f) && f.getFileName.toString.endsWith(".class")) {
      val classNode = AsmUtils.classFromBytes(Files.readAllBytes(f))
      c.visitClassSignature(classNode.signature)
      classNode.methods.iterator().asScala.map(_.signature).foreach(c.visitMethodSignature)
      classNode.fields.iterator().asScala.map(_.signature).foreach(c.visitFieldSignature)
    }
  }

  @Test
  @Ignore("manually run test")
  def allJars(): Unit = {
    // for i in $(find /Users/jz/.ivy2/cache -name jars); do find $i -name '*.jar' | head -1; done > /tmp/jars.txt
    import java.nio.file._
    import collection.JavaConverters._
    val allJars = Files.readAllLines(Paths.get("/tmp/jars.txt")).asScala
    for (path <- allJars) {
      var currentClass: Path = null
      try {
        import java.nio.file._
        import scala.collection.JavaConverters._
        val zipfile = Paths.get(path)
        println(path)
        val fs = FileSystems.newFileSystem(zipfile, null)
        val root = fs.getRootDirectories.iterator().next()
        val contents = Files.walk(root).iterator().asScala.toList
        for (f <- contents if Files.isRegularFile(f) && f.getFileName.toString.endsWith(".class")) {
          currentClass = f
          val classNode = AsmUtils.classFromBytes(Files.readAllBytes(f))
          c.visitClassSignature(classNode.signature)
          classNode.methods.iterator().asScala.map(_.signature).foreach(c.visitMethodSignature)
          classNode.fields.iterator().asScala.map(_.signature).foreach(c.visitFieldSignature)
        }
      } catch {
        case t: Throwable =>
          println("currentClass = " + currentClass)
          t.printStackTrace()
      }
    }
  }
}
