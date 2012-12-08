import org.scalacheck._
import Prop._
import Gen._
import Arbitrary._

import scala.reflect.runtime.universe._
import Flag._

object Test extends Properties("reflection extractors") {

  val genFlag = oneOf(
    TRAIT, INTERFACE, MUTABLE, MACRO, DEFERRED, ABSTRACT, FINAL, SEALED,
    IMPLICIT, LAZY, OVERRIDE, PRIVATE, PROTECTED, LOCAL, CASE, ABSOVERRIDE,
    BYNAMEPARAM, PARAM, COVARIANT, CONTRAVARIANT, DEFAULTPARAM, PRESUPER,
    DEFAULTINIT
  )
  val genModifiers = for(flag <- genFlag) yield Modifiers(flag)
  val genTermName = for(name <- arbitrary[String]) yield TermName(name)
  val genTypeName = for(name <- arbitrary[String]) yield TypeName(name)
  implicit val arbTermName: Arbitrary[TermName] = Arbitrary(genTermName)
  implicit val arbTypeName: Arbitrary[TypeName] = Arbitrary(genTypeName)
  implicit val arbMods: Arbitrary[Modifiers] = Arbitrary(genModifiers)

  property("extract termname") = forAll { (name: TermName) =>
    val TermName(s) = name
    s == name.toString
  }

  property("extract typename") = forAll { (name: TypeName) =>
    val TypeName(s) = name
    s == name.toString
  }

  property("extract modifiers") = forAll { (mods: Modifiers) =>
    val Modifiers(flags, _, _) = mods
    flags == mods.flags
  }
}