package scala.tools.nsc.tasty

object TastyFormat {

  final val header: Array[Int] = Array(0x5C, 0xA1, 0xAB, 0x1F)
  val MajorVersion: Int = 20
  val MinorVersion: Int = 0

  /** Tags used to serialize names */
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

    final val OBJECTCLASS = 23       // The name of an object class (or: module class) `<name>$`.

    final val SIGNED = 63            // A pair of a name and a signature, used to identify
                                     // possibly overloaded methods.
  }
  object NameTags extends NameTags

  // Position header

  final val SOURCE = 4

 // AST tags
  // Cat. 1:    tag

  final val firstSimpleTreeTag = UNITconst
  final val UNITconst = 2
  final val FALSEconst = 3
  final val TRUEconst = 4
  final val NULLconst = 5
  final val PRIVATE = 6
  final val INTERNAL = 7
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
  final val SCALA2X = 30
  final val DEFAULTparameterized = 31
  final val STABLE = 32
  final val MACRO = 33
  final val ERASED = 34
  final val OPAQUE = 35
  final val EXTENSION = 36
  final val GIVEN = 37
  final val PARAMsetter = 38
  final val EXPORTED = 39
  final val OPEN = 40
  final val PARAMEND = 41
  final val PARAMalias = 42

  // Cat. 2:    tag Nat

  final val SHAREDterm = 50
  final val SHAREDtype = 51
  final val TERMREFdirect = 52
  final val TYPEREFdirect = 53
  final val TERMREFpkg = 54
  final val TYPEREFpkg = 55
  final val RECthis = 56
  final val BYTEconst = 57
  final val SHORTconst = 58
  final val CHARconst = 59
  final val INTconst = 60
  final val LONGconst = 61
  final val FLOATconst = 62
  final val DOUBLEconst = 63
  final val STRINGconst = 64
  final val IMPORTED = 65
  final val RENAMED = 66

  // Cat. 3:    tag AST

  final val THIS = 80
  final val QUALTHIS = 81
  final val CLASSconst = 82
  final val ENUMconst = 83
  final val BYNAMEtype = 84
  final val BYNAMEtpt = 85
  final val NEW = 86
  final val THROW = 87
  final val IMPLICITarg = 88
  final val PRIVATEqualified = 89
  final val PROTECTEDqualified = 90
  final val RECtype = 91
  final val SINGLETONtpt = 92
  final val BOUNDED = 93

  // Cat. 4:    tag Nat AST

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

  // Cat. 5:    tag Length ...

  final val PACKAGE = 128
  final val VALDEF = 129
  final val DEFDEF = 130
  final val TYPEDEF = 131
  final val IMPORT = 132
  final val TYPEPARAM = 133
  final val PARAM = 134
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
  final val ORtype = 167
  final val POLYtype = 169
  final val TYPELAMBDAtype = 170
  final val LAMBDAtpt = 171
  final val PARAMtype = 172
  final val ANNOTATION = 173
  final val TERMREFin = 174
  final val TYPEREFin = 175

  final val METHODtype = 180
  final val ERASEDMETHODtype = 181
  final val GIVENMETHODtype = 182
  final val ERASEDGIVENMETHODtype = 183
  final val IMPLICITMETHODtype = 184

  final val MATCHtype = 190
  final val MATCHtpt = 191

  def methodTypeTag(isContextual: Boolean, isImplicit: Boolean, isErased: Boolean): Int = {
    val implicitOffset =
      if (isContextual) 2
      else if (isImplicit) { assert(!isErased); 4 }
      else 0
    val erasedOffset = if (isErased) 1 else 0
    METHODtype + erasedOffset + implicitOffset
  }

  final val HOLE = 255

  final val firstNatTreeTag = SHAREDterm
  final val firstASTTreeTag = THIS
  final val firstNatASTTreeTag = IDENT
  final val firstLengthTreeTag = PACKAGE

  /** Useful for debugging */
  def isLegalTag(tag: Int): Boolean =
    firstSimpleTreeTag <= tag && tag <= PARAMalias ||
    firstNatTreeTag <= tag && tag <= RENAMED ||
    firstASTTreeTag <= tag && tag <= BOUNDED ||
    firstNatASTTreeTag <= tag && tag <= NAMEDARG ||
    firstLengthTreeTag <= tag && tag <= MATCHtpt ||
    tag == HOLE

  def isParamTag(tag: Int): Boolean = tag == PARAM || tag == TYPEPARAM

  def isModifierTag(tag: Int): Boolean = tag match {
    case PRIVATE
       | INTERNAL
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
       | ENUM
       | LOCAL
       | SYNTHETIC
       | ARTIFACT
       | MUTABLE
       | FIELDaccessor
       | CASEaccessor
       | COVARIANT
       | CONTRAVARIANT
       | SCALA2X
       | DEFAULTparameterized
       | STABLE
       | EXTENSION
       | PARAMsetter
       | PARAMalias
       | EXPORTED
       | OPEN
       | ANNOTATION
       | PRIVATEqualified
       | PROTECTEDqualified => true
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
       | BIND => true
    case _ => false
  }

  def astTagToString(tag: Int): String = tag match {
    case UNITconst => "UNITconst"
    case FALSEconst => "FALSEconst"
    case TRUEconst => "TRUEconst"
    case NULLconst => "NULLconst"
    case PRIVATE => "PRIVATE"
    case INTERNAL => "INTERNAL"
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
    case ENUM => "ENUM"
    case LOCAL => "LOCAL"
    case SYNTHETIC => "SYNTHETIC"
    case ARTIFACT => "ARTIFACT"
    case MUTABLE => "MUTABLE"
    case FIELDaccessor => "FIELDaccessor"
    case CASEaccessor => "CASEaccessor"
    case COVARIANT => "COVARIANT"
    case CONTRAVARIANT => "CONTRAVARIANT"
    case SCALA2X => "SCALA2X"
    case DEFAULTparameterized => "DEFAULTparameterized"
    case STABLE => "STABLE"
    case EXTENSION => "EXTENSION"
    case GIVEN => "GIVEN"
    case PARAMsetter => "PARAMsetter"
    case EXPORTED => "EXPORTED"
    case OPEN => "OPEN"
    case PARAMEND => "PARAMEND"
    case PARAMalias => "PARAMalias"

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
    case TYPEPARAM => "TYPEPARAM"
    case PARAM => "PARAM"
    case IMPORTED => "IMPORTED"
    case RENAMED => "RENAMED"
    case BOUNDED => "BOUNDED"
    case APPLY => "APPLY"
    case TYPEAPPLY => "TYPEAPPLY"
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
    case ENUMconst => "ENUMconst"
    case SINGLETONtpt => "SINGLETONtpt"
    case SUPERtype => "SUPERtype"
    case TERMREFin => "TERMREFin"
    case TYPEREFin => "TYPEREFin"

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
    case ERASEDMETHODtype => "ERASEDMETHODtype"
    case GIVENMETHODtype => "GIVENMETHODtype"
    case ERASEDGIVENMETHODtype => "ERASEDGIVENMETHODtype"
    case IMPLICITMETHODtype => "IMPLICITMETHODtype"
    case TYPELAMBDAtype => "TYPELAMBDAtype"
    case LAMBDAtpt => "LAMBDAtpt"
    case MATCHtype => "MATCHtype"
    case MATCHtpt => "MATCHtpt"
    case PARAMtype => "PARAMtype"
    case ANNOTATION => "ANNOTATION"
    case PRIVATEqualified => "PRIVATEqualified"
    case PROTECTEDqualified => "PROTECTEDqualified"
    case HOLE => "HOLE"
  }

  /** @return If non-negative, the number of leading references (represented as nats) of a length/trees entry.
   *          If negative, minus the number of leading non-reference trees.
   */
  def numRefs(tag: Int): Int = tag match {
    case VALDEF | DEFDEF | TYPEDEF | TYPEPARAM | PARAM | NAMEDARG | RETURN | BIND |
         SELFDEF | REFINEDtype | TERMREFin | TYPEREFin | HOLE => 1
    case RENAMED | PARAMtype => 2
    case POLYtype | TYPELAMBDAtype |
         METHODtype | ERASEDMETHODtype |
         GIVENMETHODtype | ERASEDGIVENMETHODtype |
         IMPLICITMETHODtype => -1
    case _ => 0
  }
}
