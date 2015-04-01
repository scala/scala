/*
 * filter: inliner warnings; re-run with
 */
import scala.tools.nsc._
import scala.tools.partest.CompilerTest
import scala.collection.{ mutable, immutable, generic }

object Test extends CompilerTest {
  import global._
  import rootMirror._
  import definitions._

  override def code = """
package extest {
  trait Bippy[A <: AnyRef, B] { }     // wildcards
  trait BippyLike[A <: AnyRef, B <: List[A], This <: BippyLike[A, B, This] with Bippy[A, B]]  // no wildcards
  trait BippyBud[A <: AnyRef, B, C <: List[A]]

  trait Cov01[+A <: AnyRef, +B] { }
  trait Cov02[+A <: AnyRef,  B] { }
  trait Cov03[+A <: AnyRef, -B] { }
  trait Cov04[ A <: AnyRef, +B] { }
  trait Cov05[ A <: AnyRef,  B] { }
  trait Cov06[ A <: AnyRef, -B] { }
  trait Cov07[-A <: AnyRef, +B] { }
  trait Cov08[-A <: AnyRef,  B] { }
  trait Cov09[-A <: AnyRef, -B] { }

  trait Cov11[+A <: AnyRef, +B <: List[_]] { }
  trait Cov12[+A <: AnyRef,  B <: List[_]] { }
  trait Cov13[+A <: AnyRef, -B <: List[_]] { }
  trait Cov14[ A <: AnyRef, +B <: List[_]] { }
  trait Cov15[ A <: AnyRef,  B <: List[_]] { }
  trait Cov16[ A <: AnyRef, -B <: List[_]] { }
  trait Cov17[-A <: AnyRef, +B <: List[_]] { }
  trait Cov18[-A <: AnyRef,  B <: List[_]] { }
  trait Cov19[-A <: AnyRef, -B <: List[_]] { }

  trait Cov21[+A, +B] { }
  trait Cov22[+A,  B] { }
  trait Cov23[+A, -B] { }
  trait Cov24[ A, +B] { }
  trait Cov25[ A,  B] { }
  trait Cov26[ A, -B] { }
  trait Cov27[-A, +B] { }
  trait Cov28[-A,  B] { }
  trait Cov29[-A, -B] { }

  trait Cov31[+A, +B, C <: ((A, B))] { }
  trait Cov32[+A,  B, C <: ((A, B))] { }
  trait Cov33[+A, -B, C <: ((A, _))] { }
  trait Cov34[ A, +B, C <: ((A, B))] { }
  trait Cov35[ A,  B, C <: ((A, B))] { }
  trait Cov36[ A, -B, C <: ((A, _))] { }
  trait Cov37[-A, +B, C <: ((_, B))] { }
  trait Cov38[-A,  B, C <: ((_, B))] { }
  trait Cov39[-A, -B, C <: ((_, _))] { }

  trait Cov41[+A >: Null, +B] { }
  trait Cov42[+A >: Null,  B] { }
  trait Cov43[+A >: Null, -B] { }
  trait Cov44[ A >: Null, +B] { }
  trait Cov45[ A >: Null,  B] { }
  trait Cov46[ A >: Null, -B] { }
  trait Cov47[-A >: Null, +B] { }
  trait Cov48[-A >: Null,  B] { }
  trait Cov49[-A >: Null, -B] { }

  trait Covariant[+A <: AnyRef, +B] { }
  trait CovariantLike[+A <: AnyRef, +B <: List[A], +This <: CovariantLike[A, B, This] with Covariant[A, B]]

  trait Contra[-A >: AnyRef, -B] { }
  trait ContraLike[-A >: AnyRef, -B >: List[A]]
}
  """

  override def check(source: String, unit: global.CompilationUnit) {
    getPackage(TermName("extest")).moduleClass.info.decls.toList.filter(_.isType).map(_.initialize).sortBy(_.name.toString) foreach { clazz =>
      exitingTyper {
        clazz.info
        println(clazz.defString)
        println("    " + classExistentialType(clazz) + "\n")
      }
    }
  }
}
