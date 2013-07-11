package scala
package reflect
package internal

import Flags._

trait BuildUtils { self: SymbolTable =>
  import definitions.{TupleClass, MaxTupleArity, ScalaPackage, UnitClass}

  class BuildImpl extends BuildApi {

    def selectType(owner: Symbol, name: String): TypeSymbol =
      select(owner, newTypeName(name)).asType

    def selectTerm(owner: Symbol, name: String): TermSymbol = {
      val result = select(owner, newTermName(name)).asTerm
      if (result.isOverloaded) result.suchThat(!_.isMethod).asTerm
      else result
    }

    private def select(owner: Symbol, name: Name): Symbol = {
      val result = owner.info decl name
      if (result ne NoSymbol) result
      else
        mirrorThatLoaded(owner).missingHook(owner, name) orElse
        MissingRequirementError.notFound("%s %s in %s".format(if (name.isTermName) "term" else "type", name, owner.fullName))
    }

    def selectOverloadedMethod(owner: Symbol, name: String, index: Int): MethodSymbol = {
      val result = owner.info.decl(newTermName(name)).alternatives(index)
      if (result ne NoSymbol) result.asMethod
      else MissingRequirementError.notFound("overloaded method %s #%d in %s".format(name, index, owner.fullName))
    }

    def newFreeTerm(name: String, value: => Any, flags: Long = 0L, origin: String = null): FreeTermSymbol =
      newFreeTermSymbol(newTermName(name), value, flags, origin)

    def newFreeType(name: String, flags: Long = 0L, origin: String = null): FreeTypeSymbol =
      newFreeTypeSymbol(newTypeName(name), flags, origin)

    def newNestedSymbol(owner: Symbol, name: Name, pos: Position, flags: Long, isClass: Boolean): Symbol =
      owner.newNestedSymbol(name, pos, flags, isClass)

    def setAnnotations[S <: Symbol](sym: S, annots: List[AnnotationInfo]): S =
      sym.setAnnotations(annots)

    def setTypeSignature[S <: Symbol](sym: S, tpe: Type): S =
      sym.setTypeSignature(tpe)

    def flagsFromBits(bits: Long): FlagSet = bits

    def This(sym: Symbol): Tree = self.This(sym)

    def Select(qualifier: Tree, sym: Symbol): Select = self.Select(qualifier, sym)

    def Ident(sym: Symbol): Ident = self.Ident(sym)

    def Block(stats: List[Tree]): Block = stats match {
      case Nil => self.Block(Nil, Literal(Constant(())))
      case elem :: Nil => self.Block(Nil, elem)
      case elems => self.Block(elems.init, elems.last)
    }

    def TypeTree(tp: Type): TypeTree = self.TypeTree(tp)

    def thisPrefix(sym: Symbol): Type = sym.thisPrefix

    def setType[T <: Tree](tree: T, tpe: Type): T = { tree.setType(tpe); tree }

    def setSymbol[T <: Tree](tree: T, sym: Symbol): T = { tree.setSymbol(sym); tree }

    def mkAnnotationCtor(tree: Tree, args: List[Tree]): Tree = tree match {
      case ident: Ident => Apply(self.Select(New(ident), nme.CONSTRUCTOR: TermName), args)
      case call @ Apply(Select(New(ident: Ident), nme.CONSTRUCTOR), _) =>
        if (args.nonEmpty)
          throw new IllegalArgumentException("Can't splice annotation that already contains args with extra args, consider merging these lists together")
        call
      case _ => throw new IllegalArgumentException(s"Tree ${showRaw(tree)} isn't a correct representation of annotation, consider passing Ident as a first argument")
    }

    object FlagsAsBits extends FlagsAsBitsExtractor {
      def unapply(flags: Long): Some[Long] = Some(flags)
    }

    object TypeApplied extends TypeAppliedExtractor {
      def unapply(tree: Tree): Some[(Tree, List[Tree])] = tree match {
        case TypeApply(fun, targs) => Some((fun, targs))
        case _ => Some((tree, Nil))
      }
    }

    object Applied extends AppliedExtractor {
      def unapply(tree: Tree): Some[(Tree, List[List[Tree]])] = {
        val treeInfo.Applied(fun, targs, argss) = tree
        targs match {
          case Nil => Some((fun, argss))
          case _ => Some((TypeApply(fun, targs), argss))
        }
      }
    }

    object SyntacticClassDef extends SyntacticClassDefExtractor {
      def apply(mods: Modifiers, name: TypeName, tparams: List[TypeDef],
                constrMods: Modifiers, vparamss: List[List[ValDef]], parents: List[Tree],
                selfdef: ValDef, body: List[Tree]): Tree =
        ClassDef(mods, name, tparams, gen.mkTemplate(parents, selfdef, constrMods, vparamss, body, NoPosition))

      def unapply(tree: Tree): Option[(Modifiers, TypeName, List[TypeDef], Modifiers,
                                       List[List[ValDef]], List[Tree], ValDef, List[Tree])] = tree match {
        case ClassDef(mods, name, tparams, Template(parents, selfdef, tbody)) =>
          // extract generated fieldDefs and constructor
          val (defs, (ctor: DefDef) :: body) = tbody.splitAt(tbody.indexWhere {
            case DefDef(_, nme.CONSTRUCTOR, _, _, _, _) => true
            case _ => false
          })
          val (earlyDefs, fieldDefs) = defs.span(treeInfo.isEarlyDef)

          // undo conversion from (implicit ... ) to ()(implicit ... ) when its the only parameter section
          val vparamssRestoredImplicits = ctor.vparamss match {
            case Nil :: rest if !rest.isEmpty && !rest.head.isEmpty && rest.head.head.mods.isImplicit => rest
            case other => other
          }

          // undo flag modifications by mergeing flag info from constructor args and fieldDefs
          val modsMap = fieldDefs.map { case ValDef(mods, name, _, _) => name -> mods }.toMap
          val vparamss = mmap(vparamssRestoredImplicits) { vd =>
            val originalMods = modsMap(vd.name) | (vd.mods.flags & DEFAULTPARAM)
            atPos(vd.pos)(ValDef(originalMods, vd.name, vd.tpt, vd.rhs))
          }

          Some((mods, name, tparams, ctor.mods, vparamss, parents, selfdef, earlyDefs ::: body))
        case _ =>
          None
      }
    }

    object TupleN extends TupleNExtractor {
      def apply(args: List[Tree]): Tree = args match {
        case Nil      => Literal(Constant(()))
        case _        =>
          require(args.length <= MaxTupleArity, s"Tuples with arity bigger than $MaxTupleArity aren't supported")
          self.Apply(TupleClass(args.length).companionModule, args: _*)
      }

      def unapply(tree: Tree): Option[List[Tree]] = tree match {
        case Literal(Constant(())) =>
          Some(Nil)
        case Apply(id: Ident, args)
          if args.length <= MaxTupleArity && id.symbol == TupleClass(args.length).companionModule =>
          Some(args)
        case Apply(Select(Ident(nme.scala_), TermName(tuple)), args)
          if args.length <= MaxTupleArity && tuple == TupleClass(args.length).name =>
          Some(args)
        case _ =>
          None
      }
    }

    object TupleTypeN extends TupleNExtractor {
      def apply(args: List[Tree]): Tree = args match {
        case Nil => self.Select(self.Ident(nme.scala_), tpnme.Unit)
        case _   =>
          require(args.length <= MaxTupleArity, s"Tuples with arity bigger than $MaxTupleArity aren't supported")
          AppliedTypeTree(Ident(TupleClass(args.length)), args)
      }

      def unapply(tree: Tree): Option[List[Tree]] =  tree match {
        case Select(Ident(nme.scala_), tpnme.Unit) =>
          Some(Nil)
        case AppliedTypeTree(id: Ident, args)
          if args.length <= MaxTupleArity && id.symbol == TupleClass(args.length) =>
          Some(args)
        case AppliedTypeTree(Select(id @ Ident(nme.scala_), TermName(tuple)), args)
          if args.length <= MaxTupleArity && id.symbol == ScalaPackage && tuple == TupleClass(args.length).name =>
          Some(args)
        case _ =>
          None
      }
    }

    def RefTree(qual: Tree, sym: Symbol) = self.RefTree(qual, sym.name) setSymbol sym
  }

  val build: BuildApi = new BuildImpl
}
