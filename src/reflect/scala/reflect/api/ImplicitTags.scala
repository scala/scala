package scala
package reflect
package api

/** Tags which preserve the identity of abstract types in the face of erasure.
 *  Can be used for pattern matching, instance tests, serialization and the like.
 *  @group Tags
 */
trait ImplicitTags {
  self: Universe =>

  // Tags for Types.
  implicit val AnnotatedTypeTag: ClassTag[AnnotatedType]
  implicit val BoundedWildcardTypeTag: ClassTag[BoundedWildcardType]
  implicit val ClassInfoTypeTag: ClassTag[ClassInfoType]
  implicit val CompoundTypeTag: ClassTag[CompoundType]
  implicit val ConstantTypeTag: ClassTag[ConstantType]
  implicit val ExistentialTypeTag: ClassTag[ExistentialType]
  implicit val MethodTypeTag: ClassTag[MethodType]
  implicit val NullaryMethodTypeTag: ClassTag[NullaryMethodType]
  implicit val PolyTypeTag: ClassTag[PolyType]
  implicit val RefinedTypeTag: ClassTag[RefinedType]
  implicit val SingleTypeTag: ClassTag[SingleType]
  implicit val SingletonTypeTag: ClassTag[SingletonType]
  implicit val SuperTypeTag: ClassTag[SuperType]
  implicit val ThisTypeTag: ClassTag[ThisType]
  implicit val TypeBoundsTag: ClassTag[TypeBounds]
  implicit val TypeRefTag: ClassTag[TypeRef]
  implicit val TypeTagg: ClassTag[Type]

  // Tags for Names.
  implicit val NameTag: ClassTag[Name]
  implicit val TermNameTag: ClassTag[TermName]
  implicit val TypeNameTag: ClassTag[TypeName]

  // Tags for Scopes.
  implicit val ScopeTag: ClassTag[Scope]
  implicit val MemberScopeTag: ClassTag[MemberScope]

  // Tags for Annotations.
  implicit val AnnotationTag: ClassTag[Annotation]
  implicit val JavaArgumentTag: ClassTag[JavaArgument]
  implicit val LiteralArgumentTag: ClassTag[LiteralArgument]
  implicit val ArrayArgumentTag: ClassTag[ArrayArgument]
  implicit val NestedArgumentTag: ClassTag[NestedArgument]

  // Tags for Symbols.
  implicit val TermSymbolTag: ClassTag[TermSymbol]
  implicit val MethodSymbolTag: ClassTag[MethodSymbol]
  implicit val SymbolTag: ClassTag[Symbol]
  implicit val TypeSymbolTag: ClassTag[TypeSymbol]
  implicit val ModuleSymbolTag: ClassTag[ModuleSymbol]
  implicit val ClassSymbolTag: ClassTag[ClassSymbol]

  // Tags for misc Tree relatives.
  implicit val PositionTag: ClassTag[Position]
  implicit val ConstantTag: ClassTag[Constant]
  implicit val FlagSetTag: ClassTag[FlagSet]
  implicit val ModifiersTag: ClassTag[Modifiers]

  // Tags for Trees. WTF.
  implicit val AlternativeTag: ClassTag[Alternative]
  implicit val AnnotatedTag: ClassTag[Annotated]
  implicit val AppliedTypeTreeTag: ClassTag[AppliedTypeTree]
  implicit val ApplyTag: ClassTag[Apply]
  implicit val AssignOrNamedArgTag: ClassTag[AssignOrNamedArg]
  implicit val AssignTag: ClassTag[Assign]
  implicit val BindTag: ClassTag[Bind]
  implicit val BlockTag: ClassTag[Block]
  implicit val CaseDefTag: ClassTag[CaseDef]
  implicit val ClassDefTag: ClassTag[ClassDef]
  implicit val CompoundTypeTreeTag: ClassTag[CompoundTypeTree]
  implicit val DefDefTag: ClassTag[DefDef]
  implicit val DefTreeTag: ClassTag[DefTree]
  implicit val ExistentialTypeTreeTag: ClassTag[ExistentialTypeTree]
  implicit val FunctionTag: ClassTag[Function]
  implicit val GenericApplyTag: ClassTag[GenericApply]
  implicit val IdentTag: ClassTag[Ident]
  implicit val IfTag: ClassTag[If]
  implicit val ImplDefTag: ClassTag[ImplDef]
  implicit val ImportSelectorTag: ClassTag[ImportSelector]
  implicit val ImportTag: ClassTag[Import]
  implicit val LabelDefTag: ClassTag[LabelDef]
  implicit val LiteralTag: ClassTag[Literal]
  implicit val MatchTag: ClassTag[Match]
  implicit val MemberDefTag: ClassTag[MemberDef]
  implicit val ModuleDefTag: ClassTag[ModuleDef]
  implicit val NameTreeTag: ClassTag[NameTree]
  implicit val NewTag: ClassTag[New]
  implicit val PackageDefTag: ClassTag[PackageDef]
  implicit val RefTreeTag: ClassTag[RefTree]
  implicit val ReturnTag: ClassTag[Return]
  implicit val SelectFromTypeTreeTag: ClassTag[SelectFromTypeTree]
  implicit val SelectTag: ClassTag[Select]
  implicit val SingletonTypeTreeTag: ClassTag[SingletonTypeTree]
  implicit val StarTag: ClassTag[Star]
  implicit val SuperTag: ClassTag[Super]
  implicit val SymTreeTag: ClassTag[SymTree]
  implicit val TemplateTag: ClassTag[Template]
  implicit val TermTreeTag: ClassTag[TermTree]
  implicit val ThisTag: ClassTag[This]
  implicit val ThrowTag: ClassTag[Throw]
  implicit val TreeTag: ClassTag[Tree]
  implicit val TryTag: ClassTag[Try]
  implicit val TypTreeTag: ClassTag[TypTree]
  implicit val TypeApplyTag: ClassTag[TypeApply]
  implicit val TypeBoundsTreeTag: ClassTag[TypeBoundsTree]
  implicit val TypeDefTag: ClassTag[TypeDef]
  implicit val TypeTreeTag: ClassTag[TypeTree]
  implicit val TypedTag: ClassTag[Typed]
  implicit val UnApplyTag: ClassTag[UnApply]
  implicit val ValDefTag: ClassTag[ValDef]
  implicit val ValOrDefDefTag: ClassTag[ValOrDefDef]

  // Miscellaneous
  implicit val TreeCopierTag: ClassTag[TreeCopier]
  implicit val RuntimeClassTag: ClassTag[RuntimeClass]
  implicit val MirrorTag: ClassTag[Mirror]
}
