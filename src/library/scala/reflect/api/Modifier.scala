package scala.reflect.api

import collection.{ immutable, mutable }

sealed abstract class Modifier {
  def name: String
  def isKeyword: Boolean
  def sourceString: String = if (isKeyword) "`" + name + "`" else name

  override def equals(that: Any) = this eq that.asInstanceOf[AnyRef]
  override def hashCode = name.hashCode
  override def toString = name
}
final class SymbolModifier private (val name: String, val isKeyword: Boolean) extends Modifier {
  def this(name: String) = this(name, false)
}
final class SourceModifier private (val name: String) extends Modifier {
  def isKeyword = true
}

object SymbolModifier {
  private val seen = mutable.ListBuffer[SymbolModifier]()
  private[api] def apply(name: String): SymbolModifier = {
    val mod = name match {
      case "case" | "trait" => new SymbolModifier(name, isKeyword = true)
      case _                => new SymbolModifier(name)
    }
    seen += mod
    mod
  }
  private[api] def all = seen.toList
}
object SourceModifier {
  private val seen = mutable.ListBuffer[SourceModifier]()
  private[api] def apply(name: String): SourceModifier = {
    val mod = new SourceModifier(name)
    seen += mod
    mod
  }
  private[api] def all = seen.toList
}

object Modifier extends immutable.Set[Modifier] {
  val `abstract`       = SourceModifier("abstract")
  val `final`          = SourceModifier("final")
  val `implicit`       = SourceModifier("implicit")
  val `lazy`           = SourceModifier("lazy")
  val `macro`          = SourceModifier("macro")
  val `override`       = SourceModifier("override")
  val `private`        = SourceModifier("private")
  val `protected`      = SourceModifier("protected")
  val `sealed`         = SourceModifier("sealed")

  val `case`           = SymbolModifier("case")
  val `trait`          = SymbolModifier("trait")
  val abstractOverride = SymbolModifier("abstractOverride")
  val bynameParameter  = SymbolModifier("bynameParameter")
  val caseAccessor     = SymbolModifier("caseAccessor")
  val contravariant    = SymbolModifier("contravariant")
  val covariant        = SymbolModifier("covariant")
  val defaultInit      = SymbolModifier("defaultInit")
  val defaultParameter = SymbolModifier("defaultParameter")
  val deferred         = SymbolModifier("deferred")
  val interface        = SymbolModifier("interface")
  val java             = SymbolModifier("java")
  val local            = SymbolModifier("local")
  val mutable          = SymbolModifier("mutable")
  val paramAccessor    = SymbolModifier("paramAccessor")
  val parameter        = SymbolModifier("parameter")
  val preSuper         = SymbolModifier("preSuper")
  val static           = SymbolModifier("static")

  val sourceModifiers: Set[SourceModifier] = SourceModifier.all.toSet
  val symbolModifiers: Set[SymbolModifier] = SymbolModifier.all.toSet
  val allModifiers: Set[Modifier]          = sourceModifiers ++ symbolModifiers
  def values                               = allModifiers

  def contains(key: Modifier) = allModifiers(key)
  def iterator                = allModifiers.iterator
  def -(elem: Modifier)       = allModifiers - elem
  def +(elem: Modifier)       = allModifiers + elem
}
