import scala.reflect.runtime.universe._

trait Overloads {
  // makes sure noone erases to Any or AnyRef
  def test(x: AnyRef) = "AnyRef"
  def test(x: Annotation) = "Annotation"
  def test(x: Constant) = "Constant"
  def test(x: Mirror) = "Mirror"
  def test(x: Name) = "Name"
  def test(x: TermName) = "TermName"
  def test(x: TypeName) = "TypeName"
  def test(x: Position) = "Position"
  def test(x: Scope) = "Scope"
  def test(x: MemberScope) = "MemberScope"
  def test(x: Symbol) = "Symbol"
  def test(x: TermSymbol) = "TermSymbol"
  def test(x: TypeSymbol) = "TypeSymbol"
  def test(x: MethodSymbol) = "MethodSymbol"
  def test(x: ModuleSymbol) = "ModuleSymbol"
  def test(x: ClassSymbol) = "ClassSymbol"
  def test(x: FreeTermSymbol) = "FreeTermSymbol"
  def test(x: FreeTypeSymbol) = "FreeTypeSymbol"
  def test(x: Type) = "Type"
  def test(x: SingletonType) = "SingletonType"
  def test(x: ThisType) = "ThisType"
  def test(x: SingleType) = "SingleType"
  def test(x: SuperType) = "SuperType"
  def test(x: ConstantType) = "ConstantType"
  def test(x: TypeRef) = "TypeRef"
  def test(x: CompoundType) = "CompoundType"
  def test(x: RefinedType) = "RefinedType"
  def test(x: ClassInfoType) = "ClassInfoType"
  def test(x: MethodType) = "MethodType"
  def test(x: NullaryMethodType) = "NullaryMethodType"
  def test(x: PolyType) = "PolyType"
  def test(x: ExistentialType) = "ExistentialType"
  def test(x: AnnotatedType) = "AnnotatedType"
  def test(x: TypeBounds) = "TypeBounds"
  def test(x: BoundedWildcardType) = "BoundedWildcardType"
  def test(x: Tree) = "Tree"
  def test(x: TermTree) = "TermTree"
  def test(x: TypTree) = "TypTree"
  def test(x: SymTree) = "SymTree"
  def test(x: NameTree) = "NameTree"
  def test(x: RefTree) = "RefTree"
  def test(x: DefTree) = "DefTree"
  def test(x: MemberDef) = "MemberDef"
  def test(x: PackageDef) = "PackageDef"
  def test(x: ImplDef) = "ImplDef"
  def test(x: ClassDef) = "ClassDef"
  def test(x: ModuleDef) = "ModuleDef"
  def test(x: ValOrDefDef) = "ValOrDefDef"
  def test(x: ValDef) = "ValDef"
  def test(x: DefDef) = "DefDef"
  def test(x: TypeDef) = "TypeDef"
  def test(x: LabelDef) = "LabelDef"
  def test(x: ImportSelector) = "ImportSelector"
  def test(x: Import) = "Import"
  def test(x: Template) = "Template"
  def test(x: Block) = "Block"
  def test(x: CaseDef) = "CaseDef"
  def test(x: Alternative) = "Alternative"
  def test(x: Star) = "Star"
  def test(x: Bind) = "Bind"
  def test(x: UnApply) = "UnApply"
  def test(x: Function) = "Function"
  def test(x: Assign) = "Assign"
  def test(x: AssignOrNamedArg) = "AssignOrNamedArg"
  def test(x: If) = "If"
  def test(x: Match) = "Match"
  def test(x: Return) = "Return"
  def test(x: Try) = "Try"
  def test(x: Throw) = "Throw"
  def test(x: New) = "New"
  def test(x: Typed) = "Typed"
  def test(x: GenericApply) = "GenericApply"
  def test(x: TypeApply) = "TypeApply"
  def test(x: Apply) = "Apply"
  def test(x: Super) = "Super"
  def test(x: This) = "This"
  def test(x: Select) = "Select"
  def test(x: Ident) = "Ident"
  def test(x: ReferenceToBoxed) = "ReferenceToBoxed"
  def test(x: Literal) = "Literal"
  def test(x: Annotated) = "Annotated"
  def test(x: SingletonTypeTree) = "SingletonTypeTree"
  def test(x: SelectFromTypeTree) = "SelectFromTypeTree"
  def test(x: CompoundTypeTree) = "CompoundTypeTree"
  def test(x: AppliedTypeTree) = "AppliedTypeTree"
  def test(x: TypeBoundsTree) = "TypeBoundsTree"
  def test(x: ExistentialTypeTree) = "ExistentialTypeTree"
  def test(x: TypeTree) = "TypeTree"
  def test(x: Modifiers) = "Modifiers"
  def test(x: TreeCopier) = "TreeCopier"
}

object Test extends App with Overloads {
  val buf = scala.collection.mutable.ListBuffer[String]()
  def record(result: String): Unit = {
    println(result)
    buf += result
  }
  def check(): Unit = {
    println("checking exhaustiveness in scala.reflect.api.Universe...")
    var types = typeOf[scala.reflect.api.Universe].members.filter(sym => sym.isType && !sym.isClass).map(_.name.toString)
    types = types.filter(_ != "ModifiersCreator") // type ModifiersCreator = ModifiersExtractor
    types = types.filter(_ != "FlagSet") // type FlagSet
    types = types.filter(_ != "RuntimeClass") // type RuntimeClass = java.lang.Class[_]
    types = types.filter(_ != "JavaArgument") // deprecated
    types = types.filter(_ != "LiteralArgument") // deprecated
    types = types.filter(_ != "ArrayArgument") // deprecated
    types = types.filter(_ != "NestedArgument") // deprecated
    types = types.filter(_ != "Importer") // deprecated
    types = types.filter(_ != "Internal") // internal
    types = types.filter(_ != "Compat") // internal
    types = types.filter(_ != "BuildApi") // deprecated
    val diff = types.toList diff buf.toList
    println("uncovered type members: " + diff)
  }
  record(test(null: Annotation))
  record(test(null: Constant))
  record(test(null: Mirror))
  record(test(null: Name))
  record(test(null: TermName))
  record(test(null: TypeName))
  record(test(null: Position))
  record(test(null: Scope))
  record(test(null: MemberScope))
  record(test(null: Symbol))
  record(test(null: TermSymbol))
  record(test(null: TypeSymbol))
  record(test(null: MethodSymbol))
  record(test(null: ModuleSymbol))
  record(test(null: ClassSymbol))
  record(test(null: FreeTermSymbol))
  record(test(null: FreeTypeSymbol))
  record(test(null: Type))
  record(test(null: SingletonType))
  record(test(null: ThisType))
  record(test(null: SingleType))
  record(test(null: SuperType))
  record(test(null: ConstantType))
  record(test(null: TypeRef))
  record(test(null: CompoundType))
  record(test(null: RefinedType))
  record(test(null: ClassInfoType))
  record(test(null: MethodType))
  record(test(null: NullaryMethodType))
  record(test(null: PolyType))
  record(test(null: ExistentialType))
  record(test(null: AnnotatedType))
  record(test(null: TypeBounds))
  record(test(null: BoundedWildcardType))
  record(test(null: Tree))
  record(test(null: TermTree))
  record(test(null: TypTree))
  record(test(null: SymTree))
  record(test(null: NameTree))
  record(test(null: RefTree))
  record(test(null: DefTree))
  record(test(null: MemberDef))
  record(test(null: PackageDef))
  record(test(null: ImplDef))
  record(test(null: ClassDef))
  record(test(null: ModuleDef))
  record(test(null: ValOrDefDef))
  record(test(null: ValDef))
  record(test(null: DefDef))
  record(test(null: TypeDef))
  record(test(null: LabelDef))
  record(test(null: ImportSelector))
  record(test(null: Import))
  record(test(null: Template))
  record(test(null: Block))
  record(test(null: CaseDef))
  record(test(null: Alternative))
  record(test(null: Star))
  record(test(null: Bind))
  record(test(null: UnApply))
  record(test(null: Function))
  record(test(null: Assign))
  record(test(null: AssignOrNamedArg))
  record(test(null: If))
  record(test(null: Match))
  record(test(null: Return))
  record(test(null: Try))
  record(test(null: Throw))
  record(test(null: New))
  record(test(null: Typed))
  record(test(null: GenericApply))
  record(test(null: TypeApply))
  record(test(null: Apply))
  record(test(null: Super))
  record(test(null: This))
  record(test(null: Select))
  record(test(null: Ident))
  record(test(null: ReferenceToBoxed))
  record(test(null: Literal))
  record(test(null: Annotated))
  record(test(null: SingletonTypeTree))
  record(test(null: SelectFromTypeTree))
  record(test(null: CompoundTypeTree))
  record(test(null: AppliedTypeTree))
  record(test(null: TypeBoundsTree))
  record(test(null: ExistentialTypeTree))
  record(test(null: TypeTree))
  record(test(null: Modifiers))
  record(test(null: TreeCopier))
  check()
}