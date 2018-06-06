object Test extends App {
  val u: scala.reflect.macros.Universe = ???
  import u._
  import scala.reflect.macros.Attachments
  import scala.reflect.ClassTag
  import compat._

  val tree: Tree = ???
  val ttree: TypeTree = ???
  val stree: SymTree = ???
  val trees: List[Tree] = ???
  val mods: Modifiers = ???
  val impl: Template = ???
  val vparamss: List[List[ValDef]] = ???
  val rhs: Tree = ???
  val sym: Symbol = ???
  val tsym: TypeSymbol = ???
  val syms: List[Symbol] = ???
  val params: List[Symbol] = ???
  val tparams: List[Symbol] = ???
  val tpe: Type = ???
  val tpes: List[Type] = ???
  val tag: TypeTag[Int] = ???
  val mirror: Mirror = ???
  val decls: Scope = ???
  val pos: Position = ???
  val ann: Annotation = ???
  val anns: List[Annotation] = ???
  val const: Constant = ???
  val name: Name = ???
  val tyname: TypeName = ???
  val tename: TermName = ???
  val flags: FlagSet = ???
  val str: String = ???
  val i: Int = ???
  val b: Boolean = ???

  // abstract class BuildApi
  // abstract class ReferenceToBoxedExtractor
  // abstract trait AttachableApi
  // abstract trait FreeTermSymbolApi
  // abstract trait FreeTypeSymbolApi
  // abstract trait IdentContextApi
  // abstract trait ReferenceToBoxedApi
  // abstract trait SymTreeContextApi
  // abstract trait SymbolContextApi
  // abstract trait TreeContextApi
  // abstract trait TypeTreeContextApi
  locally(ClassDef(sym, impl): ClassDef)
  locally(DefDef(sym, mods, vparamss, rhs): DefDef)
  locally(DefDef(sym, vparamss, rhs): DefDef)
  locally(DefDef(sym, mods, rhs): DefDef)
  locally(DefDef(sym, rhs): DefDef)
  locally(DefDef(sym, (??? : List[List[Symbol]] => Tree)): DefDef)
  locally(LabelDef(sym, params, rhs): LabelDef)
  locally(ModuleDef(sym, impl): ModuleDef)
  locally(TypeDef(sym, rhs): TypeDef)
  locally(TypeDef(sym): TypeDef)
  locally(ValDef(sym, rhs): ValDef)
  locally(ValDef(sym): ValDef)
  locally(AnnotatedType(anns, tpe): AnnotatedType)
  locally(BoundedWildcardType(??? : TypeBounds): BoundedWildcardType)
  locally(TypeBounds(tpe, tpe): TypeBounds)
  locally(MethodType(params, tpe): MethodType)
  locally(RefinedType(tpes, decls): RefinedType)
  locally(RefinedType(tpes, decls, sym): RefinedType)
  locally(ClassInfoType(tpes, decls, sym): ClassInfoType)
  locally(SingleType(tpe, sym): Type)
  locally(TypeRef(tpe, sym, tpes): Type)
  locally(ExistentialType(syms, tpe): ExistentialType)
  locally(NullaryMethodType(tpe): NullaryMethodType)
  locally(ThisType(sym): Type)
  locally(SuperType(tpe, tpe): Type)
  locally(PolyType(syms, tpe): PolyType)
  locally(ConstantType(const): ConstantType)
  locally(sym.asFreeTerm: FreeTermSymbol)
  locally(sym.asFreeType: FreeTypeSymbol)
  locally(sym.attachments: Attachments { type Pos = Position })
  locally(tree.attachments: Attachments { type Pos = Position })
  locally(captureVariable(sym): Unit)
  locally(capturedVariableType(sym): Type)
  locally(sym.deSkolemize: Symbol)
  locally(tree.defineType(tpe): Tree)
  locally(existentialAbstraction(tparams, tpe): Type)
  locally(tree.freeTerms: List[FreeTermSymbol])
  locally(tree.freeTypes: List[FreeTypeSymbol])
  locally(intersectionType(tpes): Type)
  locally(intersectionType(tpes, sym): Type)
  locally(sym.isErroneous: Boolean)
  locally(sym.isFreeTerm: Boolean)
  locally(sym.isFreeType: Boolean)
  locally(sym.isLocal: Boolean)
  locally(sym.isOverride: Boolean)
  locally(tsym.isSkolem: Boolean)
  locally(treeBuild.mkAttributedIdent(sym): RefTree)
  locally(treeBuild.mkAttributedQualifier(tpe): Tree)
  locally(treeBuild.mkAttributedQualifier(tpe, sym): Tree)
  locally(treeBuild.mkAttributedRef(tpe, sym): RefTree)
  locally(treeBuild.mkAttributedRef(sym): RefTree)
  locally(treeBuild.mkAttributedSelect(tree, sym): RefTree)
  locally(treeBuild.mkAttributedThis(sym): This)
  locally(mkImporter(scala.reflect.runtime.universe): Importer{val from: scala.reflect.runtime.universe.type})
  locally(treeBuild.mkMethodCall(sym, trees): Tree)
  locally(treeBuild.mkMethodCall(sym, tpes, trees): Tree)
  locally(treeBuild.mkMethodCall(sym, name, trees): Tree)
  locally(treeBuild.mkMethodCall(sym, name, tpes, trees): Tree)
  locally(treeBuild.mkMethodCall(tree, sym, tpes, trees): Tree)
  locally(treeBuild.mkMethodCall(tree, trees): Tree)
  locally(treeBuild.mkMethodCall(tree, tpes, trees): Tree)
  locally(treeBuild.mkNullaryCall(sym, tpes): Tree)
  locally(treeBuild.mkRuntimeUniverseRef: Tree)
  locally(treeBuild.mkUnattributedRef(name): RefTree)
  locally(treeBuild.mkUnattributedRef(sym): RefTree)
  locally(sym.newClassSymbol(tyname, pos, flags): ClassSymbol)
  locally(sym.newMethodSymbol(tename, pos, flags): MethodSymbol)
  locally(sym.newModuleAndClassSymbol(name, pos, flags): (ModuleSymbol, ClassSymbol))
  locally(newScopeWith(sym, sym, sym): Scope)
  locally(sym.newTermSymbol(tename, pos, flags): TermSymbol)
  locally(sym.newTypeSymbol(tyname, pos, flags): TypeSymbol)
  locally(polyType(tparams, tpe): Type)
  locally(sym.pos: Position)
  locally((tree.pos = pos): Unit)
  locally(referenceCapturedVariable(sym): Tree)
  locally(refinedType(tpes, sym): Type)
  locally(refinedType(tpes, sym, decls, pos): Type)
  locally(sym.removeAttachment[Int]: Symbol)
  locally(tree.removeAttachment[Int]: Tree)
  locally(sym.setAnnotations(ann, ann, ann): Symbol)
  locally(sym.setName(name): Symbol)
  locally(ttree.setOriginal(tree): TypeTree)
  locally(tree.setPos(pos): Tree)
  locally(sym.setPrivateWithin(sym): Symbol)
  locally(tree.setSymbol(sym): Tree)
  locally(tree.setType(tpe): Tree)
  locally(sym.setTypeSignature(tpe): Symbol)
  locally(singleType(tpe, sym): Type)
  locally(tree.substituteSymbols(syms, syms): Tree)
  locally(tree.substituteThis(sym, tree): Tree)
  locally(tree.substituteTypes(syms, tpes): Tree)
  locally((tree.symbol = sym): Unit)
  locally((tree.tpe = tpe): Unit)
  locally(typeRef(tpe, sym, tpes): Type)
  locally(sym.updateAttachment(42): Symbol)
  locally(tree.updateAttachment(42): Tree)
  locally(FreeTermSymbolTag: ClassTag[FreeTermSymbol])
  locally((??? : FreeTermSymbol).origin)
  locally((??? : FreeTermSymbol).value)
  locally(FreeTypeSymbolTag: ClassTag[FreeTypeSymbol])
  locally((??? : FreeTypeSymbol).origin)
  locally(ReferenceToBoxedTag: ClassTag[ReferenceToBoxed])
  locally(build: BuildApi)
  locally(ReferenceToBoxed(??? : Ident): ReferenceToBoxed)
  locally((??? : ReferenceToBoxed).ident: Tree)
  locally(ReferenceToBoxed.unapply(???): Option[Ident])
  locally(build.selectType(sym, str): TypeSymbol)
  locally(build.selectTerm(sym, str): TermSymbol)
  locally(build.selectOverloadedMethod(sym, str, i): MethodSymbol)
  locally(build.newNestedSymbol(sym, name, pos, flags, b): Symbol)
  locally(build.newFreeTerm(str, i): FreeTermSymbol)
  locally(build.newFreeTerm(str, i, flags, str): FreeTermSymbol)
  locally(build.newFreeType(str): FreeTypeSymbol)
  locally(build.newFreeType(str, flags, str): FreeTypeSymbol)
  locally(build.setTypeSignature(sym, tpe): Symbol)
  locally(build.setAnnotations(sym, anns): Symbol)
  locally(build.flagsFromBits(??? : Long): FlagSet)
  locally(build.emptyValDef: ValDef)
  locally(build.This(sym): Tree)
  locally(build.Select(tree, sym): Select)
  locally(build.Ident(sym): Ident)
  locally(build.TypeTree(tpe): TypeTree)
  locally(build.thisPrefix(sym): Type)
  locally(build.setType(tree, tpe): Tree)
  locally(build.setSymbol(tree, sym): Tree)
}
