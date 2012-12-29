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
  val genModifiers =
    for(flag <- genFlag; privateWithin <- genName)
      yield Modifiers(flag, privateWithin, Nil)
  val genTermName = for(name <- arbitrary[String]) yield TermName(name)
  val genTypeName = for(name <- arbitrary[String]) yield TypeName(name)
  val genName = oneOf(genTermName, genTypeName)

  implicit val arbTermName: Arbitrary[TermName] = Arbitrary(genTermName)
  implicit val arbTypeName: Arbitrary[TypeName] = Arbitrary(genTypeName)
  implicit val arbName: Arbitrary[Name] = Arbitrary(genName)
  implicit val arbMods: Arbitrary[Modifiers] = Arbitrary(genModifiers)

  property("extract term name") = forAll { (name: TermName) =>
    val TermName(s) = name
    s == name.toString
  }

  property("extract type name") = forAll { (name: TypeName) =>
    val TypeName(s) = name
    s == name.toString
  }

  property("extract term or type name") = forAll { (name: Name) =>
    name match {
      case TermName(s) => s == name.toString
      case TypeName(s) => s == name.toString
    }
  }

  property("extract modifiers") = forAll { (mods: Modifiers) =>
    val Modifiers(flags, priv, annots) = mods
    flags == mods.flags &&
    priv == mods.privateWithin &&
    annots == mods.annotations
  }
}