/* NSC -- new Scala compiler
 * Copyright 2012-2014 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect.macros
package contexts

trait Names {
  self: Context =>

  import global._

  def freshNameCreator = globalFreshNameCreator

  def fresh(): String =
    freshName()

  def fresh(name: String): String =
    freshName(name)

  def fresh[NameType <: Name](name: NameType): NameType =
    freshName[NameType](name)

  def freshName(): String =
    freshName(nme.FRESH_PREFIX)

  def freshName(name: String): String = {
    // In comparison with the first version of freshName, current "fresh" names
    // at least can't clash with legible user-written identifiers and are much less likely to clash with each other.
    // It is still not good enough however, because the counter gets reset every time we create a new Global.
    //
    // This would most certainly cause problems if Scala featured something like introduceTopLevel,
    // but even for def macros this can lead to unexpected troubles. Imagine that one Global
    // creates a term of an anonymous type with a member featuring a "fresh" name, and then another Global
    // imports that term with a wildcard and then generates a "fresh" name of its own. Given unlucky
    // circumstances these "fresh" names might end up clashing.
    //
    // TODO: hopefully SI-7823 will provide an ultimate answer to this problem.
    // In the meanwhile I will also keep open the original issue: SI-6879 "c.freshName is broken".
    val prefix = if (name.endsWith("$")) name else name + "$" // SI-8425
    val sortOfUniqueSuffix = freshNameCreator.newName(nme.FRESH_SUFFIX)
    prefix + sortOfUniqueSuffix
  }

  def freshName[NameType <: Name](name: NameType): NameType =
    name.mapName(freshName(_)).asInstanceOf[NameType]
}