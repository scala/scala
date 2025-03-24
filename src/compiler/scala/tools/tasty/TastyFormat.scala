/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.tasty

// revision: https://github.com/scala/scala3/commit/24bff2a019afcf0f45ddfd3af6b213e7e228471c
object TastyFormat {

  /** The first four bytes of a TASTy file, followed by four values:
    * - `MajorVersion: Int` - see definition in `TastyFormat`
    * - `MinorVersion: Int` - see definition in `TastyFormat`
    * - `ExperimentalVersion: Int` - see definition in `TastyFormat`
    * - `ToolingVersion: String` - arbitrary length string representing the tool that produced the TASTy.
    */
  final val header: Array[Int] = Array(0x5C, 0xA1, 0xAB, 0x1F)

  /** Natural number. Each increment of the `MajorVersion` begins a
   *  new series of backward compatible TASTy versions.
   *
   *  A TASTy file in either the preceeding or succeeding series is
   *  incompatible with the current value.
   */
  final val MajorVersion: Int = 28

  /** Natural number. Each increment of the `MinorVersion`, within
   *  a series declared by the `MajorVersion`, breaks forward
   *  compatibility, but remains backwards compatible, with all
   *  preceding `MinorVersion`.
   */
  final val MinorVersion: Int = 6

  /** Natural Number. The `ExperimentalVersion` allows for
   *  experimentation with changes to TASTy without committing
   *  to any guarantees of compatibility.
   *
   *  A zero value indicates that the TASTy version is from a
   *  stable, final release.
   *
   *  A strictly positive value indicates that the TASTy
   *  version is experimental. An experimental TASTy file
   *  can only be read by a tool with the same version.
   *  However, tooling with an experimental TASTy version
   *  is able to read final TASTy documents if the file's
   *  `MinorVersion` is strictly less than the current value.
   */
  final val ExperimentalVersion: Int = 0

  /**This method implements a binary relation (`<:<`) between two TASTy versions.
   *
   * We label the lhs `file` and rhs `compiler`.
   * if `file <:< compiler` then the TASTy file is valid to be read.
   *
   * A TASTy version, e.g. `v := 28.0-3` is composed of three fields:
   *   - v.major == 28
   *   - v.minor == 0
   *   - v.experimental == 3
   *
   * TASTy versions have a partial order, for example,
   * `a <:< b` and `b <:< a` are both false if
   *   - `a` and `b` have different `major` fields.
   *   - `a` and `b` have the same `major` & `minor` fields,
   *     but different `experimental` fields, both non-zero.
   *
   * A TASTy version with a zero value for its `experimental` field
   * is considered to be stable. Files with a stable TASTy version
   * can be read by a compiler with an unstable TASTy version,
   * (where the compiler's TASTy version has a higher `minor` field).
   *
   * A compiler with a stable TASTy version can never read a file
   * with an unstable TASTy version.
   *
   * We follow the given algorithm:
   *
   * ```
   * (fileMajor, fileMinor, fileExperimental) match
   *   case (`compilerMajor`, `compilerMinor`, `compilerExperimental`) => true // full equality
   *   case (`compilerMajor`, minor, 0) if minor < compilerMinor       => true // stable backwards compatibility
   *   case _                                                          => false
   * ```
   * @syntax markdown
   */
  def isVersionCompatible(
    fileMajor: Int,
    fileMinor: Int,
    fileExperimental: Int,
    compilerMajor: Int,
    compilerMinor: Int,
    compilerExperimental: Int
  ): Boolean = (
    fileMajor == compilerMajor &&
      (  fileMinor == compilerMinor && fileExperimental == compilerExperimental // full equality
      || fileMinor <  compilerMinor && fileExperimental == 0 // stable backwards compatibility
    )
  )

  final val ASTsSection = "ASTs"
  final val PositionsSection = "Positions"
  final val CommentsSection = "Comments"
  final val AttributesSection = "Attributes"

  /** Tags used to serialize names, should update [[TastyFormat$.nameTagToString]] if a new constant is added */
  class NameTags {
    final val UTF8 = 1               // A simple name in UTF8 encoding.

    final val QUALIFIED = 2          // A fully qualified name `<prefix>.<suffix>`.

    final val EXPANDED = 3           // An expanded name `<prefix>$$<suffix>`,
                                     // used by Scala-2 for private names.

    final val EXPANDPREFIX = 4       // An expansion prefix `<prefix>$<suffix>`,
                                     // used by Scala-2 for private names.

    final val UNIQUE = 10            // A unique name `<name>$<num>` where `<num>`
                                     // is used only once for each `<name>`.

    final val DEFAULTGETTER = 11     // The name `<meth-name>$default$<param-num>`
                                     // of a default getter that returns a default argument.

    final val SUPERACCESSOR = 20     // The name of a super accessor `super$name` created by SuperAccesors.

    final val INLINEACCESSOR = 21    // The name of an inline accessor `inline$name`

    final val BODYRETAINER = 22      // The name of a synthetic method that retains the runtime
                                     // body of an inline method

    final val OBJECTCLASS = 23       // The name of an object class (or: module class) `<name>$`.

    final val SIGNED = 63            // A pair of a name and a signature, used to identify
                                     // possibly overloaded methods.

    final val TARGETSIGNED = 62      // A triple of a name, a targetname and a signature, used to identify
                                     // possibly overloaded methods that carry a @targetName annotation.

    // TODO swap SIGNED and TARGETSIGNED codes on next major version bump
  }
  object NameTags extends NameTags

  /**Should be kept in sync with [[NameTags]]. Converts constants to a String representing their identifier,
   * or NotANameTag(tag) if unrecognised.
   *
   * For debugging purposes when unpickling names in a TASTy file.
   */
  def nameTagToString(tag: Int) = {
    import NameTags._
    tag match {
      case UTF8 => "UTF8"
      case QUALIFIED => "QUALIFIED"
      case EXPANDED => "EXPANDED"
      case EXPANDPREFIX => "EXPANDPREFIX"
      case UNIQUE => "UNIQUE"
      case DEFAULTGETTER => "DEFAULTGETTER"
      case SUPERACCESSOR => "SUPERACCESSOR"
      case INLINEACCESSOR => "INLINEACCESSOR"
      case BODYRETAINER => "BODYRETAINER"
      case OBJECTCLASS => "OBJECTCLASS"
      case SIGNED => "SIGNED"
      case TARGETSIGNED => "TARGETSIGNED"
      case id => s"NotANameTag($id)"
    }
  }

  // Position header

  final val SOURCE = 4

  // AST tags

  // Tree Cat. 1:    tag
  final val firstSimpleTreeTag = UNITconst
  // final val ??? = 1
  final val UNITconst = 2
  final val FALSEconst = 3
  final val TRUEconst = 4
  final val NULLconst = 5
  final val PRIVATE = 6
  // final val ??? = 7
  final val PROTECTED = 8
  final val ABSTRACT = 9
  final val FINAL = 10
  final val SEALED = 11
  final val CASE = 12
  final val IMPLICIT = 13
  final val LAZY = 14
  final val OVERRIDE = 15
  final val INLINEPROXY = 16
  final val INLINE = 17
  final val STATIC = 18
  final val OBJECT = 19
  final val TRAIT = 20
  final val ENUM = 21
  final val LOCAL = 22
  final val SYNTHETIC = 23
  final val ARTIFACT = 24
  final val MUTABLE = 25
  final val FIELDaccessor = 26
  final val CASEaccessor = 27
  final val COVARIANT = 28
  final val CONTRAVARIANT = 29
  // final val ??? = 30
  final val HASDEFAULT = 31
  final val STABLE = 32
  final val MACRO = 33
  final val ERASED = 34
  final val OPAQUE = 35
  final val EXTENSION = 36
  final val GIVEN = 37
  final val PARAMsetter = 38
  final val EXPORTED = 39
  final val OPEN = 40
  final val PARAMalias = 41
  final val TRANSPARENT = 42
  final val INFIX = 43
  final val INVISIBLE = 44
  final val EMPTYCLAUSE = 45
  final val SPLITCLAUSE = 46
  final val TRACKED = 47

  // Tree Cat. 2:    tag Nat
  final val firstNatTreeTag = SHAREDterm
  final val SHAREDterm = 60
  final val SHAREDtype = 61
  final val TERMREFdirect = 62
  final val TYPEREFdirect = 63
  final val TERMREFpkg = 64
  final val TYPEREFpkg = 65
  final val RECthis = 66
  final val BYTEconst = 67
  final val SHORTconst = 68
  final val CHARconst = 69
  final val INTconst = 70
  final val LONGconst = 71
  final val FLOATconst = 72
  final val DOUBLEconst = 73
  final val STRINGconst = 74
  final val IMPORTED = 75
  final val RENAMED = 76

  // Tree Cat. 3:    tag AST
  final val firstASTTreeTag = THIS
  final val THIS = 90
  final val QUALTHIS = 91
  final val CLASSconst = 92
  final val BYNAMEtype = 93
  final val BYNAMEtpt = 94
  final val NEW = 95
  final val THROW = 96
  final val IMPLICITarg = 97
  final val PRIVATEqualified = 98
  final val PROTECTEDqualified = 99
  final val RECtype = 100
  final val SINGLETONtpt = 101
  final val BOUNDED = 102
  final val EXPLICITtpt = 103
  final val ELIDED = 104

  // Tree Cat. 4:    tag Nat AST
  final val firstNatASTTreeTag = IDENT
  final val IDENT = 110
  final val IDENTtpt = 111
  final val SELECT = 112
  final val SELECTtpt = 113
  final val TERMREFsymbol = 114
  final val TERMREF = 115
  final val TYPEREFsymbol = 116
  final val TYPEREF = 117
  final val SELFDEF = 118
  final val NAMEDARG = 119

  // Tree Cat. 5:    tag Length ...
  final val firstLengthTreeTag = PACKAGE
  final val PACKAGE = 128
  final val VALDEF = 129
  final val DEFDEF = 130
  final val TYPEDEF = 131
  final val IMPORT = 132
  final val TYPEPARAM = 133
  final val PARAM = 134
  // final val ??? = 135
  final val APPLY = 136
  final val TYPEAPPLY = 137
  final val TYPED = 138
  final val ASSIGN = 139
  final val BLOCK = 140
  final val IF = 141
  final val LAMBDA = 142
  final val MATCH = 143
  final val RETURN = 144
  final val WHILE = 145
  final val TRY = 146
  final val INLINED = 147
  final val SELECTouter = 148
  final val REPEATED = 149
  final val BIND = 150
  final val ALTERNATIVE = 151
  final val UNAPPLY = 152
  final val ANNOTATEDtype = 153
  final val ANNOTATEDtpt = 154
  final val CASEDEF = 155
  final val TEMPLATE = 156
  final val SUPER = 157
  final val SUPERtype = 158
  final val REFINEDtype = 159
  final val REFINEDtpt = 160
  final val APPLIEDtype = 161
  final val APPLIEDtpt = 162
  final val TYPEBOUNDS = 163
  final val TYPEBOUNDStpt = 164
  final val ANDtype = 165
  // final val ??? = 166
  final val ORtype = 167
  // final val ??? = 168
  final val POLYtype = 169
  final val TYPELAMBDAtype = 170
  final val LAMBDAtpt = 171
  final val PARAMtype = 172
  final val ANNOTATION = 173
  final val TERMREFin = 174
  final val TYPEREFin = 175
  final val SELECTin = 176
  final val EXPORT = 177
  final val QUOTE = 178
  final val SPLICE = 179
  final val METHODtype = 180
  final val APPLYsigpoly = 181
  final val QUOTEPATTERN = 182
  final val SPLICEPATTERN = 183

  final val MATCHtype = 190
  final val MATCHtpt = 191
  final val MATCHCASEtype = 192
  final val FLEXIBLEtype = 193

  final val HOLE = 255

  // Attributes tags

  // Attribute Category 1 (tags 1-32)  :  tag
  def isBooleanAttrTag(tag: Int): Boolean = 1 <= tag && tag <= 32
  final val SCALA2STANDARDLIBRARYattr = 1
  final val EXPLICITNULLSattr = 2
  final val CAPTURECHECKEDattr = 3
  final val WITHPUREFUNSattr = 4
  final val JAVAattr = 5
  final val OUTLINEattr = 6

  // Attribute Category 2 (tags 33-128): unassigned

  // Attribute Category 3 (tags 129-160):  tag Utf8Ref
  def isStringAttrTag(tag: Int): Boolean = 129 <= tag && tag <= 160
  final val SOURCEFILEattr = 129

  // Attribute Category 4 (tags 161-255): unassigned

  // end of Attributes tags


  /** Useful for debugging */
  def isLegalTag(tag: Int): Boolean =
    firstSimpleTreeTag <= tag && tag <= SPLITCLAUSE ||
    firstNatTreeTag <= tag && tag <= RENAMED ||
    firstASTTreeTag <= tag && tag <= BOUNDED ||
    firstNatASTTreeTag <= tag && tag <= NAMEDARG ||
    firstLengthTreeTag <= tag && tag <= FLEXIBLEtype ||
    tag == HOLE

  def isParamTag(tag: Int): Boolean = tag == PARAM || tag == TYPEPARAM

  def isModifierTag(tag: Int): Boolean = tag match {
    case PRIVATE
       | PROTECTED
       | ABSTRACT
       | FINAL
       | SEALED
       | CASE
       | IMPLICIT
       | GIVEN
       | ERASED
       | LAZY
       | OVERRIDE
       | INLINE
       | INLINEPROXY
       | MACRO
       | OPAQUE
       | STATIC
       | OBJECT
       | TRAIT
       | TRANSPARENT
       | INFIX
       | ENUM
       | LOCAL
       | SYNTHETIC
       | ARTIFACT
       | MUTABLE
       | FIELDaccessor
       | CASEaccessor
       | COVARIANT
       | CONTRAVARIANT
       | HASDEFAULT
       | STABLE
       | EXTENSION
       | PARAMsetter
       | PARAMalias
       | EXPORTED
       | OPEN
       | INVISIBLE
       | ANNOTATION
       | PRIVATEqualified
       | PROTECTEDqualified
       | TRACKED => true
    case _ => false
  }

  def isTypeTreeTag(tag: Int): Boolean = tag match {
    case IDENTtpt
       | SELECTtpt
       | SINGLETONtpt
       | REFINEDtpt
       | APPLIEDtpt
       | LAMBDAtpt
       | TYPEBOUNDStpt
       | ANNOTATEDtpt
       | BYNAMEtpt
       | MATCHtpt
       | EXPLICITtpt
       | BIND => true
    case _ => false
  }

  def astTagToString(tag: Int): String = tag match {
    case UNITconst => "UNITconst"
    case FALSEconst => "FALSEconst"
    case TRUEconst => "TRUEconst"
    case NULLconst => "NULLconst"
    case PRIVATE => "PRIVATE"
    case PROTECTED => "PROTECTED"
    case ABSTRACT => "ABSTRACT"
    case FINAL => "FINAL"
    case SEALED => "SEALED"
    case CASE => "CASE"
    case IMPLICIT => "IMPLICIT"
    case ERASED => "ERASED"
    case LAZY => "LAZY"
    case OVERRIDE => "OVERRIDE"
    case INLINE => "INLINE"
    case INLINEPROXY => "INLINEPROXY"
    case MACRO => "MACRO"
    case OPAQUE => "OPAQUE"
    case STATIC => "STATIC"
    case OBJECT => "OBJECT"
    case TRAIT => "TRAIT"
    case TRANSPARENT => "TRANSPARENT"
    case INFIX => "INFIX"
    case ENUM => "ENUM"
    case LOCAL => "LOCAL"
    case SYNTHETIC => "SYNTHETIC"
    case ARTIFACT => "ARTIFACT"
    case MUTABLE => "MUTABLE"
    case FIELDaccessor => "FIELDaccessor"
    case CASEaccessor => "CASEaccessor"
    case COVARIANT => "COVARIANT"
    case CONTRAVARIANT => "CONTRAVARIANT"
    case HASDEFAULT => "HASDEFAULT"
    case STABLE => "STABLE"
    case EXTENSION => "EXTENSION"
    case GIVEN => "GIVEN"
    case PARAMsetter => "PARAMsetter"
    case EXPORTED => "EXPORTED"
    case OPEN => "OPEN"
    case INVISIBLE => "INVISIBLE"
    case PARAMalias => "PARAMalias"
    case EMPTYCLAUSE => "EMPTYCLAUSE"
    case SPLITCLAUSE => "SPLITCLAUSE"

    case SHAREDterm => "SHAREDterm"
    case SHAREDtype => "SHAREDtype"
    case TERMREFdirect => "TERMREFdirect"
    case TYPEREFdirect => "TYPEREFdirect"
    case TERMREFpkg => "TERMREFpkg"
    case TYPEREFpkg => "TYPEREFpkg"
    case RECthis => "RECthis"
    case BYTEconst => "BYTEconst"
    case SHORTconst => "SHORTconst"
    case CHARconst => "CHARconst"
    case INTconst => "INTconst"
    case LONGconst => "LONGconst"
    case FLOATconst => "FLOATconst"
    case DOUBLEconst => "DOUBLEconst"
    case STRINGconst => "STRINGconst"
    case RECtype => "RECtype"

    case IDENT => "IDENT"
    case IDENTtpt => "IDENTtpt"
    case SELECT => "SELECT"
    case SELECTtpt => "SELECTtpt"
    case TERMREFsymbol => "TERMREFsymbol"
    case TERMREF => "TERMREF"
    case TYPEREFsymbol => "TYPEREFsymbol"
    case TYPEREF => "TYPEREF"

    case PACKAGE => "PACKAGE"
    case VALDEF => "VALDEF"
    case DEFDEF => "DEFDEF"
    case TYPEDEF => "TYPEDEF"
    case IMPORT => "IMPORT"
    case EXPORT => "EXPORT"
    case TYPEPARAM => "TYPEPARAM"
    case PARAM => "PARAM"
    case IMPORTED => "IMPORTED"
    case RENAMED => "RENAMED"
    case BOUNDED => "BOUNDED"
    case APPLY => "APPLY"
    case TYPEAPPLY => "TYPEAPPLY"
    case APPLYsigpoly => "APPLYsigpoly"
    case NEW => "NEW"
    case THROW => "THROW"
    case TYPED => "TYPED"
    case NAMEDARG => "NAMEDARG"
    case ASSIGN => "ASSIGN"
    case BLOCK => "BLOCK"
    case IF => "IF"
    case LAMBDA => "LAMBDA"
    case MATCH => "MATCH"
    case RETURN => "RETURN"
    case WHILE => "WHILE"
    case INLINED => "INLINED"
    case SELECTouter => "SELECTouter"
    case TRY => "TRY"
    case REPEATED => "REPEATED"
    case BIND => "BIND"
    case ALTERNATIVE => "ALTERNATIVE"
    case UNAPPLY => "UNAPPLY"
    case ANNOTATEDtype => "ANNOTATEDtype"
    case ANNOTATEDtpt => "ANNOTATEDtpt"
    case CASEDEF => "CASEDEF"
    case IMPLICITarg => "IMPLICITarg"
    case TEMPLATE => "TEMPLATE"
    case SELFDEF => "SELFDEF"
    case THIS => "THIS"
    case QUALTHIS => "QUALTHIS"
    case SUPER => "SUPER"
    case CLASSconst => "CLASSconst"
    case SINGLETONtpt => "SINGLETONtpt"
    case SUPERtype => "SUPERtype"
    case TERMREFin => "TERMREFin"
    case TYPEREFin => "TYPEREFin"
    case SELECTin => "SELECTin"

    case REFINEDtype => "REFINEDtype"
    case REFINEDtpt => "REFINEDtpt"
    case APPLIEDtype => "APPLIEDtype"
    case APPLIEDtpt => "APPLIEDtpt"
    case TYPEBOUNDS => "TYPEBOUNDS"
    case TYPEBOUNDStpt => "TYPEBOUNDStpt"
    case ANDtype => "ANDtype"
    case ORtype => "ORtype"
    case BYNAMEtype => "BYNAMEtype"
    case BYNAMEtpt => "BYNAMEtpt"
    case POLYtype => "POLYtype"
    case METHODtype => "METHODtype"
    case TYPELAMBDAtype => "TYPELAMBDAtype"
    case LAMBDAtpt => "LAMBDAtpt"
    case MATCHtype => "MATCHtype"
    case MATCHCASEtype => "MATCHCASEtype"
    case MATCHtpt => "MATCHtpt"
    case PARAMtype => "PARAMtype"
    case FLEXIBLEtype => "FLEXIBLEtype"
    case ANNOTATION => "ANNOTATION"
    case PRIVATEqualified => "PRIVATEqualified"
    case PROTECTEDqualified => "PROTECTEDqualified"
    case EXPLICITtpt => "EXPLICITtpt"
    case ELIDED => "ELIDED"
    case QUOTE => "QUOTE"
    case SPLICE => "SPLICE"
    case QUOTEPATTERN => "QUOTEPATTERN"
    case SPLICEPATTERN => "SPLICEPATTERN"
    case HOLE => "HOLE"
  }

  def attributeTagToString(tag: Int): String = tag match {
    case SCALA2STANDARDLIBRARYattr => "SCALA2STANDARDLIBRARYattr"
    case EXPLICITNULLSattr => "EXPLICITNULLSattr"
    case CAPTURECHECKEDattr => "CAPTURECHECKEDattr"
    case WITHPUREFUNSattr => "WITHPUREFUNSattr"
    case JAVAattr => "JAVAattr"
    case OUTLINEattr => "OUTLINEattr"
    case SOURCEFILEattr => "SOURCEFILEattr"
  }

  /** @return If non-negative, the number of leading references (represented as nats) of a length/trees entry.
   *          If negative, minus the number of leading non-reference trees.
   */
  def numRefs(tag: Int): Int = tag match {
    case VALDEF | DEFDEF | TYPEDEF | TYPEPARAM | PARAM | NAMEDARG | RETURN | BIND |
         SELFDEF | REFINEDtype | TERMREFin | TYPEREFin | SELECTin | HOLE => 1
    case RENAMED | PARAMtype => 2
    case POLYtype | TYPELAMBDAtype | METHODtype => -1
    case _ => 0
  }
}
