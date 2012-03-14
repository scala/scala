/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package internal

import java.security.MessageDigest
import scala.io.Codec
import Chars.isOperatorPart

/** A trait to encapsulate name mangling.  It's intended for the
 *  values and methods involved in assembling names out of other names,
 *  and not for simple synthetically named locals.
 */
trait NameManglers {
  self: SymbolTable =>

  trait NameManglingCommon {
    self: CommonNames =>

    val MODULE_SUFFIX_STRING = NameTransformer.MODULE_SUFFIX_STRING
    val NAME_JOIN_STRING     = NameTransformer.NAME_JOIN_STRING

    val MODULE_SUFFIX_NAME: TermName = newTermName(MODULE_SUFFIX_STRING)
    val NAME_JOIN_NAME: TermName     = newTermName(NAME_JOIN_STRING)

    def flattenedName(segments: Name*): NameType = compactedString(segments mkString NAME_JOIN_STRING)

    /**
     * COMPACTIFY
     *
     * The hashed name has the form (prefix + marker + md5 + marker + suffix), where
     *   - prefix/suffix.length = MaxNameLength / 4
     *   - md5.length = 32
     *
     * We obtain the formula:
     *
     *   FileNameLength = 2*(MaxNameLength / 4) + 2.marker.length + 32 + 6
     *
     * (+6 for ".class"). MaxNameLength can therefore be computed as follows:
     */
    private final val marker = "$$$$"
    private final val MaxNameLength = math.min(
      settings.maxClassfileName.value - 6,
      2 * (settings.maxClassfileName.value - 6 - 2*marker.length - 32)
    )
    private lazy val md5 = MessageDigest.getInstance("MD5")
    private def toMD5(s: String, edge: Int) = {
      val prefix = s take edge
      val suffix = s takeRight edge

      val cs = s.toArray
      val bytes = Codec toUTF8 cs
      md5 update bytes
      val md5chars = md5.digest() map (b => (b & 0xFF).toHexString) mkString

      prefix + marker + md5chars + marker + suffix
    }
    private def compactedString(s: String) =
      if (s.length <= MaxNameLength) s
      else toMD5(s, MaxNameLength / 4)
  }

  trait TypeNameMangling extends NameManglingCommon {
    self: tpnme.type =>

  }

  trait TermNameMangling extends NameManglingCommon {
    self: nme.type =>

    val IMPL_CLASS_SUFFIX             = "$class"
    val LOCALDUMMY_PREFIX             = "<local "   // owner of local blocks
    val PROTECTED_PREFIX              = "protected$"
    val PROTECTED_SET_PREFIX          = PROTECTED_PREFIX + "set"
    val SINGLETON_SUFFIX              = ".type"
    val SUPER_PREFIX_STRING           = "super$"
    val TRAIT_SETTER_SEPARATOR_STRING = "$_setter_$"
    val SETTER_SUFFIX: TermName = encode("_=")

    @deprecated("2.10.0", "Use SPECIALIZED_SUFFIX")
    def SPECIALIZED_SUFFIX_STRING = SPECIALIZED_SUFFIX.toString
    @deprecated("2.10.0", "Use SPECIALIZED_SUFFIX")
    def SPECIALIZED_SUFFIX_NAME: TermName = SPECIALIZED_SUFFIX.toTermName

    def isConstructorName(name: Name)       = name == CONSTRUCTOR || name == MIXIN_CONSTRUCTOR
    def isExceptionResultName(name: Name)   = name startsWith EXCEPTION_RESULT_PREFIX
    def isImplClassName(name: Name)         = name endsWith IMPL_CLASS_SUFFIX
    def isLocalDummyName(name: Name)        = name startsWith LOCALDUMMY_PREFIX
    def isLocalName(name: Name)             = name endsWith LOCAL_SUFFIX_STRING
    def isLoopHeaderLabel(name: Name)       = (name startsWith WHILE_PREFIX) || (name startsWith DO_WHILE_PREFIX)
    def isProtectedAccessorName(name: Name) = name startsWith PROTECTED_PREFIX
    def isSuperAccessorName(name: Name)     = name startsWith SUPER_PREFIX_STRING
    def isReplWrapperName(name: Name)       = name containsName INTERPRETER_IMPORT_WRAPPER
    def isSetterName(name: Name)            = name endsWith SETTER_SUFFIX
    def isTraitSetterName(name: Name)       = isSetterName(name) && (name containsName TRAIT_SETTER_SEPARATOR_STRING)
    def isSingletonName(name: Name)         = name endsWith SINGLETON_SUFFIX
    def isModuleName(name: Name)            = name endsWith MODULE_SUFFIX_NAME

    def isOpAssignmentName(name: Name) = name match {
      case raw.NE | raw.LE | raw.GE | EMPTY => false
      case _                                =>
        name.endChar == '=' && name.startChar != '=' && isOperatorPart(name.startChar)
    }

    /** The expanded setter name of `name` relative to this class `base`
     */
    def expandedSetterName(name: TermName, base: Symbol): TermName =
      expandedName(name, base, separator = TRAIT_SETTER_SEPARATOR_STRING)

    /** If `name` is an expandedName name, the original name.
     *  Otherwise `name` itself.
     */
    def originalName(name: Name): Name = {
      var i = name.length
      while (i >= 2 && !(name(i - 1) == '$' && name(i - 2) == '$')) i -= 1
      if (i >= 2) {
        while (i >= 3 && name(i - 3) == '$') i -= 1
        name.subName(i, name.length)
      } else name
    }

    def unspecializedName(name: Name): Name = (
      if (name endsWith SPECIALIZED_SUFFIX)
        name.subName(0, name.lastIndexOf('m') - 1)
      else name
    )

    def macroMethodName(name: Name) = {
      val base = if (name.isTypeName) nme.TYPEkw else nme.DEFkw
      base append nme.MACRO append name
    }

    /** Return the original name and the types on which this name
     *  is specialized. For example,
     *  {{{
     *     splitSpecializedName("foo$mIcD$sp") == ('foo', "I", "D")
     *  }}}
     *  `foo$mIcD$sp` is the name of a method specialized on two type
     *  parameters, the first one belonging to the method itself, on Int,
     *  and another one belonging to the enclosing class, on Double.
     */
    def splitSpecializedName(name: Name): (Name, String, String) =
      if (name endsWith SPECIALIZED_SUFFIX) {
        val name1 = name dropRight SPECIALIZED_SUFFIX.length
        val idxC  = name1 lastIndexOf 'c'
        val idxM  = name1 lastIndexOf 'm'

        (name1.subName(0, idxM - 1),
         name1.subName(idxC + 1, name1.length).toString,
         name1.subName(idxM + 1, idxC).toString)
      } else
        (name, "", "")

    def getterName(name: TermName): TermName     = if (isLocalName(name)) localToGetter(name) else name
    def getterToLocal(name: TermName): TermName  = name append LOCAL_SUFFIX_STRING
    def getterToSetter(name: TermName): TermName = name append SETTER_SUFFIX
    def localToGetter(name: TermName): TermName  = name dropRight LOCAL_SUFFIX_STRING.length

    def dropLocalSuffix(name: Name): Name  = if (name endsWith ' ') name dropRight 1 else name

    def setterToGetter(name: TermName): TermName = {
      val p = name.pos(TRAIT_SETTER_SEPARATOR_STRING)
      if (p < name.length)
        setterToGetter(name drop (p + TRAIT_SETTER_SEPARATOR_STRING.length))
      else
        name.subName(0, name.length - SETTER_SUFFIX.length)
    }

    def defaultGetterName(name: Name, pos: Int): TermName = {
      val prefix = if (isConstructorName(name)) "init" else name
      newTermName(prefix + DEFAULT_GETTER_STRING + pos)
    }
    def defaultGetterToMethod(name: Name): TermName = {
      val p = name.pos(DEFAULT_GETTER_STRING)
      if (p < name.length) name.toTermName.subName(0, p)
      else name.toTermName
    }

    // This isn't needed at the moment since I fixed $class$1 but
    // I expect it will be at some point.
    //
    // def anonNumberSuffix(name: Name): Name = {
    //   ("" + name) lastIndexOf '$' match {
    //     case -1   => nme.EMPTY
    //     case idx  =>
    //       val s = name drop idx
    //       if (s.toString forall (_.isDigit)) s
    //       else nme.EMPTY
    //   }
    // }

    def stripModuleSuffix(name: Name): Name = (
      if (isModuleName(name)) name dropRight MODULE_SUFFIX_STRING.length else name
    )

    def dropSingletonName(name: Name): TypeName = name dropRight SINGLETON_SUFFIX.length toTypeName
    def singletonName(name: Name): TypeName     = name append SINGLETON_SUFFIX toTypeName
    def implClassName(name: Name): TypeName     = name append IMPL_CLASS_SUFFIX toTypeName
    def interfaceName(implname: Name): TypeName = implname dropRight IMPL_CLASS_SUFFIX.length toTypeName
    def localDummyName(clazz: Symbol): TermName = newTermName(LOCALDUMMY_PREFIX + clazz.name + ">")
    def superName(name: Name): TermName         = newTermName(SUPER_PREFIX_STRING + name)

    /** The name of an accessor for protected symbols. */
    def protName(name: Name): TermName = newTermName(PROTECTED_PREFIX + name)

    /** The name of a setter for protected symbols. Used for inherited Java fields. */
    def protSetterName(name: Name): TermName = newTermName(PROTECTED_SET_PREFIX + name)
  }
}
