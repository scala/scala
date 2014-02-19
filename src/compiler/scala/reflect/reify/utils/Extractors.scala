package scala.reflect.reify
package utils

trait Extractors {
  self: Utils =>

  import global._
  import definitions._
  import Flag._

  // Example of a reified tree for `reify(List(1, 2))`:
  // (also contains an example of a reified type as a third argument to the constructor of Expr)
  // {
  //   val $u: scala.reflect.runtime.universe.type = scala.reflect.runtime.`package`.universe;
  //   val $m: $u.Mirror = $u.runtimeMirror(Test.this.getClass().getClassLoader());
  //   $u.Expr[List[Int]]($m, {
  //     final class $treecreator1 extends scala.reflect.api.TreeCreator {
  //       def <init>(): $treecreator1 = {
  //         $treecreator1.super.<init>();
  //         ()
  //       };
  //       def apply[U >: Nothing <: scala.reflect.api.Universe with Singleton]($m$untyped: scala.reflect.api.Mirror[U]): U#Tree = {
  //         val $u: U = $m$untyped.universe;
  //         val $m: $u.Mirror = $m$untyped.asInstanceOf[$u.Mirror];
  //         $u.Apply($u.Select($u.Select($u.build.This($m.staticPackage("scala.collection.immutable").moduleClass), $u.newTermName("List")), $u.newTermName("apply")), List($u.Literal($u.Constant(1)), $u.Literal($u.Constant(2))))
  //       }
  //     };
  //     new $treecreator1()
  //   })($u.TypeTag[List[Int]]($m, {
  //     final class $typecreator1 extends scala.reflect.api.TypeCreator {
  //       def <init>(): $typecreator1 = {
  //         $typecreator1.super.<init>();
  //         ()
  //       };
  //       def apply[U >: Nothing <: scala.reflect.api.Universe with Singleton]($m$untyped: scala.reflect.api.Mirror[U]): U#Type = {
  //         val $u: U = $m$untyped.universe;
  //         val $m: $u.Mirror = $m$untyped.asInstanceOf[$u.Mirror];
  //         $u.TypeRef($u.ThisType($m.staticPackage("scala.collection.immutable").moduleClass), $m.staticClass("scala.collection.immutable.List"), List($m.staticClass("scala.Int").toTypeConstructor))
  //       }
  //     };
  //     new $typecreator1()
  //   }))
  // }

  private def mkCreator(flavor: TypeName, symtab: SymbolTable, rtree: Tree): Tree = {
    val tparamu = newTypeName("U")
    val (reifierBase, reifierName, reifierTpt, reifierUniverse) = flavor match {
      case tpnme.REIFY_TYPECREATOR_PREFIX => (TypeCreatorClass, nme.apply, SelectFromTypeTree(Ident(tparamu), tpnme.Type), ApiUniverseClass)
      case tpnme.REIFY_TREECREATOR_PREFIX => (TreeCreatorClass, nme.apply, SelectFromTypeTree(Ident(tparamu), tpnme.Tree), ApiUniverseClass)
      case _ => throw new Error(s"unexpected flavor $flavor")
    }
    val reifierBody = {
      def gc(symtab: SymbolTable): SymbolTable = {
        def loop(symtab: SymbolTable): SymbolTable = {
          def extractNames(tree: Tree) = tree.collect{ case ref: RefTree => ref.name }.toSet
          val usedNames = extractNames(rtree) ++ symtab.syms.flatMap(sym => extractNames(symtab.symDef(sym)))
          symtab filterAliases { case (_, name) => usedNames(name) }
        }
        var prev = symtab
        var next = loop(symtab)
        while (next.syms.length < prev.syms.length) {
          prev = next
          next = loop(prev)
        }
        next
      }

      val universeAlias = ValDef(NoMods, nme.UNIVERSE_SHORT, Ident(tparamu), Select(Ident(nme.MIRROR_UNTYPED), nme.universe))
      val mirrorAlias = ValDef(NoMods, nme.MIRROR_SHORT, Select(Ident(nme.UNIVERSE_SHORT), tpnme.Mirror), TypeApply(Select(Ident(nme.MIRROR_UNTYPED), nme.asInstanceOf_), List(Select(Ident(nme.UNIVERSE_SHORT), tpnme.Mirror))))
      val trimmedSymtab = if (hasReifier) gc(symtab) else symtab
      Block(universeAlias :: mirrorAlias :: trimmedSymtab.encode, rtree)
    }
    val tpec = ClassDef(
      Modifiers(FINAL),
      newTypeName(global.currentUnit.fresh.newName(flavor.toString)),
      List(),
      Template(List(Ident(reifierBase)),
      noSelfType,
      List(
        DefDef(NoMods, nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(())))),
        DefDef(NoMods,
          reifierName,
          List(TypeDef(Modifiers(PARAM), tparamu, List(), TypeBoundsTree(Ident(NothingClass), CompoundTypeTree(Template(List(Ident(reifierUniverse), Ident(SingletonClass)), noSelfType, List()))))),
          List(List(ValDef(Modifiers(PARAM), nme.MIRROR_UNTYPED, AppliedTypeTree(Ident(MirrorClass), List(Ident(tparamu))), EmptyTree))),
          reifierTpt, reifierBody))))
    Block(tpec, ApplyConstructor(Ident(tpec.name), List()))
  }

  private def mkWrapper(universe: Tree, mirror: Tree, wrappee: Tree): Tree = {
    val universeAlias = ValDef(NoMods, nme.UNIVERSE_SHORT, SingletonTypeTree(universe), universe)
    val mirrorAlias = ValDef(NoMods, nme.MIRROR_SHORT, Select(Ident(nme.UNIVERSE_SHORT), tpnme.Mirror), mirror orElse mkDefaultMirrorRef(global)(universe, typer))
    Block(List(universeAlias, mirrorAlias), wrappee)
  }

  // if we're reifying a MethodType, we can't use it as a type argument for TypeTag ctor
  // http://groups.google.com/group/scala-internals/browse_thread/thread/2d7bb85bfcdb2e2
  private def mkTarg(tpe: Type): Tree = (
    if ((tpe eq null) || !isUseableAsTypeArg(tpe)) TypeTree(AnyTpe)
    else TypeTree(tpe)
  )

  object ReifiedTree {
    def apply(universe: Tree, mirror: Tree, symtab: SymbolTable, rtree: Tree, tpe: Type, rtpe: Tree, concrete: Boolean): Tree = {
      val tagFactory = if (concrete) nme.TypeTag else nme.WeakTypeTag
      val tagCtor = TypeApply(Select(Select(Ident(nme.UNIVERSE_SHORT), tagFactory), nme.apply), List(mkTarg(tpe)))
      val exprCtor = TypeApply(Select(Select(Ident(nme.UNIVERSE_SHORT), nme.Expr), nme.apply), List(mkTarg(tpe)))
      val tagArgs = List(Ident(nme.MIRROR_SHORT), mkCreator(tpnme.REIFY_TYPECREATOR_PREFIX, symtab, rtpe))
      val unwrapped = Apply(Apply(exprCtor, List(Ident(nme.MIRROR_SHORT), mkCreator(tpnme.REIFY_TREECREATOR_PREFIX, symtab, rtree))), List(Apply(tagCtor, tagArgs)))
      mkWrapper(universe, mirror, unwrapped)
    }

    def unapply(tree: Tree): Option[(Tree, Tree, SymbolTable, Tree, Type, Tree, Boolean)] = tree match {
      case Block(
        List(udef @ ValDef(_, _, _, universe), mdef @ ValDef(_, _, _, mirror)),
        Apply(
          Apply(TypeApply(_, List(ttpe @ TypeTree())), List(_, Block(List(ClassDef(_, _, _, Template(_, _, List(_, DefDef(_, _, _, _, _, Block(_ :: _ :: symbolTable1, rtree)))))), _))),
          // todo. doesn't take into account optimizations such as $u.TypeTag.Int or the upcoming closure optimization
          List(Apply(TypeApply(tagFactory @ Select(_, _), _), List(_, Block(List(ClassDef(_, _, _, Template(_, _, List(_, DefDef(_, _, _, _, _, Block(_ :: _ :: symbolTable2, rtpe)))))), _))))))
        if udef.name == nme.UNIVERSE_SHORT && mdef.name == nme.MIRROR_SHORT =>
          val tagFlavor = tagFactory match {
            case Select(Select(_, tagFlavor), _) => tagFlavor
            case Select(_, tagFlavor) => tagFlavor
          }
          Some((universe, mirror, SymbolTable(symbolTable1 ++ symbolTable2), rtree, ttpe.tpe, rtpe, tagFlavor == nme.TypeTag))
      case _ =>
        None
    }
  }

  object ReifiedType {
    def apply(universe: Tree, mirror: Tree, symtab: SymbolTable, tpe: Type, rtpe: Tree, concrete: Boolean) = {
      val tagFactory = if (concrete) nme.TypeTag else nme.WeakTypeTag
      val ctor = TypeApply(Select(Select(Ident(nme.UNIVERSE_SHORT), tagFactory), nme.apply), List(mkTarg(tpe)))
      val args = List(Ident(nme.MIRROR_SHORT), mkCreator(tpnme.REIFY_TYPECREATOR_PREFIX, symtab, rtpe))
      val unwrapped = Apply(ctor, args)
      mkWrapper(universe, mirror, unwrapped)
    }

    def unapply(tree: Tree): Option[(Tree, Tree, SymbolTable, Type, Tree, Boolean)] = tree match {
      case Block(
        List(udef @ ValDef(_, _, _, universe), mdef @ ValDef(_, _, _, mirror)),
        // todo. doesn't take into account optimizations such as $u.TypeTag.Int or the upcoming closure optimization
        Apply(TypeApply(tagFactory @ Select(_, _), List(ttpe @ TypeTree())), List(_, Block(List(ClassDef(_, _, _, Template(_, _, List(_, DefDef(_, _, _, _, _, Block(_ :: _ :: symtab, rtpe)))))), _))))
        if udef.name == nme.UNIVERSE_SHORT && mdef.name == nme.MIRROR_SHORT =>
          val tagFlavor = tagFactory match {
            case Select(Select(_, tagFlavor), _) => tagFlavor
            case Select(_, tagFlavor) => tagFlavor
          }
          Some((universe, mirror, SymbolTable(symtab), ttpe.tpe, rtpe, tagFlavor == nme.TypeTag))
      case _ =>
        None
    }
  }

  object TreeSplice {
    def apply(splicee: Tree): Tree =
      Select(splicee, ExprSplice)

    def unapply(tree: Tree): Option[Tree] = tree match {
      case Select(splicee, _) if tree.symbol != NoSymbol && tree.symbol == ExprSplice =>
        Some(splicee)
      case _ =>
        None
    }
  }

  // abstract over possible additional .apply select
  // which is sometimes inserted after desugaring of calls
  object ApplyCall {
    def unapply(tree: Tree): Option[(Tree, List[Tree])] = tree match {
      case Apply(Select(id, nme.apply), args) => Some((id, args))
      case Apply(id, args) => Some((id, args))
      case _ => None
    }
  }

  sealed abstract class FreeDefExtractor(acceptTerms: Boolean, acceptTypes: Boolean) {
    def unapply(tree: Tree): Option[(Tree, TermName, Tree, Long, String)] = {
      def acceptFreeTermFactory(name: Name) = {
        (acceptTerms && name == nme.newFreeTerm) ||
        (acceptTypes && name == nme.newFreeType)
      }
      tree match {
        case
          ValDef(_, name, _, Apply(
            Select(Select(Select(uref1 @ Ident(_), internal1), rs1), freeTermFactory),
            _ :+
            ApplyCall(Select(Select(Select(uref2 @ Ident(_), internal2), rs2), flagsRepr), List(Literal(Constant(flags: Long)))) :+
            Literal(Constant(origin: String))))
        if uref1.name == nme.UNIVERSE_SHORT && internal1 == nme.internal && rs1 == nme.reificationSupport && acceptFreeTermFactory(freeTermFactory) &&
           uref2.name == nme.UNIVERSE_SHORT && internal2 == nme.internal && rs2 == nme.reificationSupport && flagsRepr == nme.FlagsRepr =>
          Some((uref1, name, reifyBinding(tree), flags, origin))
        case _ =>
          None
      }
    }
  }
  object FreeDef extends FreeDefExtractor(acceptTerms = true, acceptTypes = true)
  object FreeTermDef extends FreeDefExtractor(acceptTerms = true, acceptTypes = false)
  object FreeTypeDef extends FreeDefExtractor(acceptTerms = false, acceptTypes = true)

  object FreeRef {
    def unapply(tree: Tree): Option[(Tree, TermName)] = tree match {
      case Apply(Select(Select(Select(uref @ Ident(_), internal), rs), mkIdent), List(Ident(name: TermName)))
      if internal == nme.internal && rs == nme.reificationSupport && mkIdent == nme.mkIdent && name.startsWith(nme.REIFY_FREE_PREFIX) =>
        Some((uref, name))
      case _ =>
        None
    }
  }

  object SymDef {
    def unapply(tree: Tree): Option[(Tree, TermName, Long, Boolean)] = tree match {
      case
        ValDef(_, name, _, Apply(
          Select(Select(Select(uref1 @ Ident(_), internal1), rs1), newNestedSymbol),
          List(
            _,
            _,
            _,
            ApplyCall(Select(Select(Select(uref2 @ Ident(_), internal2), rs2), flagsRepr), List(Literal(Constant(flags: Long)))),
            Literal(Constant(isClass: Boolean)))))
      if uref1.name == nme.UNIVERSE_SHORT && internal1 == nme.internal && rs1 == nme.reificationSupport && newNestedSymbol == nme.newNestedSymbol &&
         uref2.name == nme.UNIVERSE_SHORT && internal2 == nme.internal && rs2 == nme.reificationSupport && flagsRepr == nme.FlagsRepr =>
        Some((uref1, name, flags, isClass))
      case _ =>
        None
    }
  }

  object TypeRefToFreeType {
    def unapply(tree: Tree): Option[TermName] = tree match {
      case Apply(Select(Select(uref @ Ident(_), typeRef), apply), List(Select(_, noSymbol), Ident(freeType: TermName), nil))
      if (uref.name == nme.UNIVERSE_SHORT && typeRef == nme.TypeRef && noSymbol == nme.NoSymbol && freeType.startsWith(nme.REIFY_FREE_PREFIX)) =>
        Some(freeType)
      case _ =>
        None
    }
  }

  object BoundTerm {
    def unapply(tree: Tree): Option[Tree] = tree match {
      case Select(_, name) if name.isTermName =>
        Some(tree)
      case Ident(name) if name.isTermName =>
        Some(tree)
      case This(_) =>
        Some(tree)
      case _ =>
        None
    }
  }

  object BoundType {
    def unapply(tree: Tree): Option[RefTree] = tree match {
      case tree @ Select(_, name) if name.isTypeName =>
        Some(tree)
      case tree @ SelectFromTypeTree(_, _) =>
        Some(tree)
      case tree @ Ident(name) if name.isTypeName =>
        Some(tree)
      case _ =>
        None
    }
  }
}
