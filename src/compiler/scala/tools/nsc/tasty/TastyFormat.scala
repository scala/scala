package scala.tools.nsc.tasty

/************************************************************

The files in this directory was adapted from https://github.com/lampepfl/dotty.git commit #af66579749e567c8319863cc25cc7e6eac093114

  - this file adapted from https://github.com/lampepfl/dotty/blob/master/compiler/src/dotty/tools/dotc/core/tasty/TastyFormat.scala

Notation:

We use BNF notation. Terminal symbols start with at least two
consecutive upper case letters. Each terminal is represented as a
single byte tag. Non-terminals are mixed case. Prefixes of the form
lower case letter*_ are for explanation of semantic content only, they
can be dropped without changing the grammar.

Micro-syntax:

  LongInt       = Digit* StopDigit        -- big endian 2's complement, value fits in a Long w/o overflow
  Int           = LongInt                 -- big endian 2's complement, fits in an Int w/o overflow
  Nat           = LongInt                 -- non-negative value, fits in an Int without overflow
  Digit         = 0 | ... | 127
  StopDigit     = 128 | ... | 255         -- value = digit - 128

Macro-format:

  File          = Header majorVersion_Nat minorVersion_Nat UUID
                  nameTable_Length Name* Section*
  Header        = 0x5CA1AB1F
  UUID          = Byte*16                 -- random UUID

  Section       = NameRef Length Bytes
  Length        = Nat                     -- length of rest of entry in bytes

  Name          = UTF8              Length UTF8-CodePoint*
                  QUALIFIED         Length qualified_NameRef selector_NameRef               -- A.B
                  EXPANDED          Length qualified_NameRef selector_NameRef               -- A$$B, semantically a NameKinds.ExpandedName
                  EXPANDPREFIX      Length qualified_NameRef selector_NameRef               -- A$B, prefix of expanded name, see NamedKinds.ExpandPrefixName

                  UNIQUE            Length separator_NameRef uniqid_Nat underlying_NameRef? -- Unique name A<separator><number>
                  DEFAULTGETTER     Length underlying_NameRef index_Nat                     -- DefaultGetter$<number>
                  VARIANT           Length underlying_NameRef variance_Nat                  -- variance 0: +A, variance = 1: -A

                  SUPERACCESSOR     Length underlying_NameRef                               -- super$A
                  INLINEACCESSOR    Length underlying_NameRef                               -- inline$A
                  OBJECTCLASS       Length underlying_NameRef                               -- A$  (name of the module class for module A)

                  SIGNED            Length original_NameRef resultSig_NameRef ParamSig*     -- name + signature

  ParamSig      = Int // If negative, the absolute value represents the length of a type parameter section
                      // If positive, this is a NameRef for the fully qualified name of a term parameter.

  NameRef       = Nat                    // ordinal number of name in name table, starting from 1.

Note: Unqualified names in the name table are strings. The context decides whether a name is
a type-name or a term-name. The same string can represent both.

Standard-Section: "ASTs" TopLevelStat*

  TopLevelStat  = PACKAGE        Length Path TopLevelStat*                         -- package path { topLevelStats }
                  Stat

  Stat          = Term
                  ValOrDefDef
                  TYPEDEF        Length NameRef (type_Term | Template) Modifier*   -- modifiers type name (= type | bounds)  |  moifiers class name template
                  IMPORT         Length [GIVEN] qual_Term Selector*                -- import given? qual selectors
  ValOrDefDef   = VALDEF         Length NameRef type_Term rhs_Term? Modifier*      -- modifiers val name : type (= rhs)?
                  DEFDEF         Length NameRef TypeParam* Params* returnType_Term rhs_Term?
                                        Modifier*                                  -- modifiers def name [typeparams] paramss : returnType (= rhs)?
  Selector      = IMPORTED              name_NameRef                               -- name
                  RENAMED               to_NameRef                                 -- => name
                  BOUNDED               type_Term?                                 -- for type

  TypeParam     = TYPEPARAM      Length NameRef type_Term Modifier*                -- modifiers name bounds
  Param         = PARAM          Length NameRef type_Term rhs_Term? Modifier*      -- modifiers name : type (= rhs_Term)?. `rhsTerm` is present in the case of an aliased class parameter
                  PARAMEND                                                         -- ends a parameter clause
                   																                                 -- needed if previous parameter clause is empty or another parameter clause follows
  Template      = TEMPLATE       Length TypeParam* Param* parent_Term* Self? Stat* -- [typeparams] paramss extends parents { self => stats }, where Stat* always starts with the primary constructor.
  Self          = SELFDEF               selfName_NameRef selfType_Term             -- selfName : selfType

  Term          = Path                                                             -- Paths represent both types and terms
                  IDENT                 NameRef Type                               -- Used when term ident’s type is not a TermRef
                  SELECT                possiblySigned_NameRef qual_Term           -- qual.name
                  QUALTHIS              typeIdent_Tree                             -- id.this, different from THIS in that it contains a qualifier ident with position.
                  NEW                   clsType_Term                               -- new cls
                  THROW                 throwableExpr_Term                         -- throw throwableExpr
                  NAMEDARG              paramName_NameRef arg_Term                 -- paramName = arg
                  APPLY          Length fn_Term arg_Term*                          -- fn(args)
                  TYPEAPPLY      Length fn_Term arg_Type*                          -- fn[args]
                  SUPER          Length this_Term mixinTypeIdent_Tree?             -- super[mixin]
                  TYPED          Length expr_Term ascriptionType_Term              -- expr: ascription
                  ASSIGN         Length lhs_Term rhs_Term                          -- lhs = rhs
                  BLOCK          Length expr_Term Stat*                            -- { stats; expr }
                  INLINED        Length expr_Term call_Term? ValOrDefDef*          -- Inlined code from call, with given body `expr` and given bindings
                  LAMBDA         Length meth_Term target_Type?                     -- Closure over method `f` of type `target` (omitted id `target` is a function type)
                  IF             Length [INLINE] cond_Term then_Term else_Term     -- inline? if cond then thenPart else elsePart
                  MATCH          Length (IMPLICIT | [INLINE] sel_Term) CaseDef*    -- (inline? sel | implicit) match caseDefs
                  TRY            Length expr_Term CaseDef* finalizer_Term?         -- try expr catch {casdeDef} (finally finalizer)?
                  RETURN         Length meth_ASTRef expr_Term?                     -- return expr?,  `methASTRef` is method from which is returned
                  WHILE          Length cond_Term body_Term                        -- while cond do body
                  REPEATED       Length elem_Type elem_Term*                       -- Varargs argument of type `elem`
                  SELECTouter    Length levels_Nat qual_Term underlying_Type       -- Follow `levels` outer links, starting from `qual`, with given `underlying` type
    -- patterns:
                  BIND           Length boundName_NameRef patType_Type pat_Term    -- name @ pat, wherev `patType` is the type of the bound symbol
                  ALTERNATIVE    Length alt_Term*                                  -- alt1 | ... | altn   as a pattern
                  UNAPPLY        Length fun_Term ImplicitArg* pat_Type pat_Term*   -- Unapply node `fun(_: pat_Type)(implicitArgs)` flowing into patterns `pat`.
    -- type trees:
                  IDENTtpt              NameRef Type                               -- Used for all type idents
                  SELECTtpt             NameRef qual_Term                          -- qual.name
                  SINGLETONtpt          ref_Term                                   -- ref.type
                  REFINEDtpt     Length underlying_Term refinement_Stat*           -- underlying {refinements}
                  APPLIEDtpt     Length tycon_Term arg_Term*                       -- tycon [args]
                  LAMBDAtpt      Length TypeParam* body_Term                       -- [TypeParams] => body
                  TYPEBOUNDStpt  Length low_Term high_Term?                        -- >: low <: high
                  ANNOTATEDtpt   Length underlying_Term fullAnnotation_Term        -- underlying @ annotation
                  MATCHtpt       Length bound_Term? sel_Term CaseDef*              -- sel match { CaseDef } where `bound` is optional upper bound of all rhs
                  BYNAMEtpt             underlying_Term                            -- => underlying
                  SHAREDterm            term_ASTRef                                -- Link to previously serialized term
                  HOLE           Length idx_Nat arg_Tree*                          -- Hole where a splice goes with sequence number idx, splice is applied to arguments `arg`s

  CaseDef       = CASEDEF        Length pat_Term rhs_Tree guard_Tree?              -- case pat if guard => rhs
  ImplicitArg   = IMPLICITARG           arg_Term                                   -- implicit unapply argument

  ASTRef        = Nat                                                              -- Byte position in AST payload

  Path          = Constant
                  TERMREFdirect         sym_ASTRef                                 -- A reference to a local symbol (without a prefix). Reference is to definition node of symbol.
                  TERMREFsymbol         sym_ASTRef qual_Type                       -- A reference `qual.sym` to a local member with prefix `qual`
                  TERMREFpkg            fullyQualified_NameRef                     -- A reference to a package member with given fully qualified name
                  TERMREF               possiblySigned_NameRef qual_Type           -- A reference `qual.name` to a non-local member
                  TERMREFin      Length possiblySigned_NameRef qual_Type namespace_Type -- A reference `qual.name` to a non-local member that's private in `namespace`
                  THIS                  clsRef_Type                                -- cls.this
                  RECthis               recType_ASTRef                             -- The `this` in a recursive refined type `recType`.
                  SHAREDtype            path_ASTRef                                -- link to previously serialized path

  Constant      = UNITconst                                                        -- ()
                  FALSEconst                                                       -- false
                  TRUEconst                                                        -- true
                  BYTEconst             Int                                        -- A byte number
                  SHORTconst            Int                                        -- A short number
                  CHARconst             Nat                                        -- A character
                  INTconst              Int                                        -- An int number
                  LONGconst             LongInt                                    -- A long number
                  FLOATconst            Int                                        -- A float number
                  DOUBLEconst           LongInt                                    -- A double number
                  STRINGconst           NameRef                                    -- A string literal
                  NULLconst                                                        -- null
                  CLASSconst            Type                                       -- classOf[Type]
                  ENUMconst             Path                                       -- An enum constant

  Type          = Path                                                             -- Paths represent both types and terms
                  TYPEREFdirect         sym_ASTRef                                 -- A reference to a local symbol (without a prefix). Reference is to definition node of symbol.
                  TYPEREFsymbol         sym_ASTRef qual_Type                       -- A reference `qual.sym` to a local member with prefix `qual`
                  TYPEREFpkg            fullyQualified_NameRef                     -- A reference to a package member with given fully qualified name
                  TYPEREF               NameRef qual_Type                          -- A reference `qual.name` to a non-local member
                  TYPEREFin      Length NameRef qual_Type namespace_Type           -- A reference `qual.name` to a non-local member that's private in `namespace`.
                  RECtype               parent_Type                                -- A wrapper for recursive refined types
                  SUPERtype      Length this_Type underlying_Type                  -- A super type reference to `underlying`
                  REFINEDtype    Length underlying_Type refinement_NameRef info_Type -- underlying { refinement_name : info }
                  APPLIEDtype    Length tycon_Type arg_Type*                       -- tycon[args]
                  TYPEALIAS             alias_Type                                 -- = alias
                  TYPEBOUNDS     Length low_Type high_Type                         -- >: low <: high
                  ANNOTATEDtype  Length underlying_Type annotation_Term            -- underlying @ annotation
                  ANDtype        Length left_Type right_Type                       -- left & right
                  ORtype         Length left_Type right_Type                       -- lefgt | right
                  MATCHtype      Length bound_Type sel_Type case_Type*             -- sel match {cases} with optional upper `bound`
                  BIND           Length boundName_NameRef bounds_Type              -- boundName @ bounds,  for type-variables defined in a type pattern
                  BYNAMEtype            underlying_Type                            -- => underlying
                  PARAMtype      Length binder_ASTRef paramNum_Nat                 -- A reference to parameter # paramNum in lambda type `binder`
                  POLYtype       Length result_Type NamesTypes                     -- A polymorphic method type `[NamesTypes]result`, used in refinements
                  METHODtype     Length result_Type NamesTypes                     -- A method type `(NamesTypes)result`, needed for refinements
                  ERASEDMETHODtype      Length result_Type NamesTypes              -- A method type `erased (NamesTypes)result`, needed for refinements
                  GIVENMETHODtype       Length result_Type NamesTypes              -- A method type `given (NamesTypes)result`, needed for refinements
                  ERASEDGIVENMETHODtype Length result_Type NamesTypes              -- A method type `given erased (NamesTypes)result`, needed for refinements
                  IMPLICITMETHODtype    Length result_Type NamesTypes              -- A method type `(implicit NamesTypes)result`, needed for refinements
  // TODO: remove ERASEDIMPLICITMETHODtype
                  TYPELAMBDAtype Length result_Type NamesTypes                     -- A type lambda `[NamesTypes] => result`, variance encoded using VARIANT names
                  SHAREDtype            type_ASTRef                                -- link to previously serialized type
  NamesTypes    = NameType*
  NameType      = paramName_NameRef typeOrBounds_ASTRef                            -- `termName : type`  or  `typeName bounds`

  Modifier      = PRIVATE                                                          -- private
                  INTERNAL                                                         -- package private (not yet used)
                  PROTECTED                                                        -- protected
                  PRIVATEqualified     qualifier_Type                              -- private[qualifier]    (to be dropped(?)
                  PROTECTEDqualified   qualifier_Type                              -- protecred[qualifier]  (to be dropped(?)
                  ABSTRACT                                                         -- abstract
                  FINAL                                                            -- final
                  SEALED                                                           -- sealed
                  CASE                                                             -- case  (for classes or objects)
                  IMPLICIT                                                         -- implicit
                  GIVEN                                                            -- given
                  ERASED                                                           -- erased
                  LAZY                                                             -- lazy
                  OVERRIDE                                                         -- override
                  OPAQUE                                                           -- opaque
                  INLINE                                                           -- inline
                  MACRO                                                            -- Inline method containing toplevel splices
                  INLINEPROXY                                                      -- Symbol of binding with an argument to an inline method as rhs (TODO: do we still need this?)
                  STATIC                                                           -- Mapped to static Java member
                  OBJECT                                                           -- An object or its class
                  TRAIT                                                            -- A trait
                  ENUM                                                             -- A enum class or enum case
                  LOCAL                                                            -- private[this] or protected[this], used in conjunction with PRIVATE or PROTECTED
                  SYNTHETIC                                                        -- Generated by Scala compiler
                  ARTIFACT                                                         -- To be tagged Java Synthetic
                  MUTABLE                                                          -- A var
                  FIELDaccessor                                                    -- A getter or setter (note: the corresponding field is not serialized)
                  CASEaccessor                                                     -- A getter for a case class parameter
                  COVARIANT                                                        -- A type parameter marked “+”
                  CONTRAVARIANT                                                    -- A type parameter marked “-”
                  SCALA2X                                                          -- Imported from Scala2.x
                  DEFAULTparameterized                                             -- Method with default parameters (default arguments are separate methods with DEFAULTGETTER names)
                  STABLE                                                           -- Method that is assumed to be stable, i.e. its applications are legal paths
                  EXTENSION                                                        -- An extension method
                  PARAMsetter                                                      -- The setter part `x_=` of a var parameter `x` which itself is pickled as a PARAM
                  EXPORTED                                                         -- An export forwarder
                  Annotation

  Annotation    = ANNOTATION     Length tycon_Type fullAnnotation_Term             -- An annotation, given (class) type of constructor, and full application tree

Note: Tree tags are grouped into 5 categories that determine what follows, and thus allow to compute the size of the tagged tree in a generic way.

  Category 1 (tags 1-49)   :  tag
  Category 2 (tags 50-79)  :  tag Nat
  Category 3 (tags 80-109) :  tag AST
  Category 4 (tags 110-127):  tag Nat AST
  Category 5 (tags 128-255):  tag Length <payload>

Standard Section: "Positions" Assoc*

  Assoc         = Header offset_Delta? offset_Delta? point_Delta?
                | SOURCE nameref_Int
  Header        = addr_Delta +              // in one Nat: difference of address to last recorded node << 3 +
                  hasStartDiff +            // one bit indicating whether there follows a start address delta << 2
                  hasEndDiff +              // one bit indicating whether there follows an end address delta << 1
                  hasPoint                  // one bit indicating whether the new position has a point (i.e ^ position)
                                            // Nodes which have the same positions as their parents are omitted.
                                            // offset_Deltas give difference of start/end offset wrt to the
                                            // same offset in the previously recorded node (or 0 for the first recorded node)
  Delta         = Int                       // Difference between consecutive offsets,
  SOURCE        = 4                         // Impossible as header, since addr_Delta = 0 implies that we refer to the
                                            // same tree as the previous one, but then hasStartDiff = 1 implies that
                                            // the tree's range starts later than the range of itself.

All elements of a position section are serialized as Ints

Standard Section: "Comments" Comment*

  Comment       = Length Bytes LongInt      // Raw comment's bytes encoded as UTF-8, followed by the comment's coordinates.


**************************************************************************************/

object TastyFormat {

  final val header: Array[Int] = Array(0x5C, 0xA1, 0xAB, 0x1F)
  val MajorVersion: Int = 18
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

    final val VARIANT = 12           // A name `+<name>` or `-<name>` indicating
                                     // a co- or contra-variant parameter of a type lambda.

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
  // final val OPEN = 40
  final val PARAMEND = 41

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
  final val TYPEALIAS = 92
  final val SINGLETONtpt = 93
  final val BOUNDED = 94

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
  final val PARAM = 135
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
    firstSimpleTreeTag <= tag && tag <= PARAMEND ||
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
       | EXPORTED
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
    // case OPEN => "OPEN"
    case PARAMEND => "PARAMEND"

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
    case TYPEALIAS => "TYPEALIAS"
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
