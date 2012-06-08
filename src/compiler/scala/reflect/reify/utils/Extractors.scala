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
  //   val $u: reflect.runtime.universe.type = scala.reflect.runtime.`package`.universe;
  //   val $m: $u.Mirror = $u.runtimeMirror(Test.this.getClass().getClassLoader());
  //   $u.Expr[List[Int]]($m, {
  //     final class $treecreator1 extends scala.reflect.base.TreeCreator {
  //       def <init>(): $treecreator1 = {
  //         $treecreator1.super.<init>();
  //         ()
  //       };
  //       def apply[U >: Nothing <: scala.reflect.base.Universe with Singleton]($m$untyped: scala.reflect.base.MirrorOf[U]): U#Tree = {
  //         val $u: scala.reflect.api.Universe = $m$untyped.universe.asInstanceOf[scala.reflect.api.Universe];
  //         val $m: $u.Mirror = $m$untyped.asInstanceOf[$u.Mirror];
  //         applyImpl($m).asInstanceOf[U#Tree];
  //       }
  //       def applyImpl[U >: Nothing <: scala.reflect.api.Universe with Singleton]($m$untyped: scala.reflect.base.MirrorOf[U]): U#Tree = {
  //         val $u: U = $m$untyped.universe;
  //         val $m: $u.Mirror = $m$untyped.asInstanceOf[$u.Mirror];
  //         $u.Apply($u.Select($u.Select($u.build.This($m.staticModule("scala.collection.immutable").moduleClass), $u.newTermName("List")), $u.newTermName("apply")), List($u.Literal($u.Constant(1)), $u.Literal($u.Constant(2))))
  //       }
  //     };
  //     new $treecreator1()
  //   })($u.TypeTag[List[Int]]($m, {
  //     final class $typecreator1 extends scala.reflect.base.TypeCreator {
  //       def <init>(): $typecreator1 = {
  //         $typecreator1.super.<init>();
  //         ()
  //       };
  //       def apply[U >: Nothing <: scala.reflect.base.Universe with Singleton]($m$untyped: scala.reflect.base.MirrorOf[U]): U#Type = {
  //         val $u: U = $m$untyped.universe;
  //         val $m: $u.Mirror = $m$untyped.asInstanceOf[$u.Mirror];
  //         $u.TypeRef($u.ThisType($m.staticModule("scala.collection.immutable").moduleClass), $m.staticClass("scala.collection.immutable.List"), List($m.staticClass("scala.Int").asTypeConstructor))
  //       }
  //     };
  //     new $typecreator1()
  //   }))
  // }

  private def mkCreator(flavor: TypeName, symtab: SymbolTable, rtree: Tree): Tree = {
    val tparamu = newTypeName("U")
    val (reifierBase, reifierName, reifierTpt, reifierUniverse) = flavor match {
      case tpnme.REIFY_TYPECREATOR_PREFIX => (TypeCreatorClass, nme.apply, SelectFromTypeTree(Ident(tparamu), tpnme.Type), BaseUniverseClass)
      case tpnme.REIFY_TREECREATOR_PREFIX => (TreeCreatorClass, nme.applyImpl, SelectFromTypeTree(Ident(BaseUniverseClass), tpnme.Tree), ApiUniverseClass)
      case _ => throw new Error(s"unexpected flavor $flavor")
    }
    val reifierPreamble = flavor match {
      case tpnme.REIFY_TYPECREATOR_PREFIX => Nil
      case tpnme.REIFY_TREECREATOR_PREFIX => List[Tree](
        DefDef(NoMods,
          nme.apply,
          List(TypeDef(Modifiers(PARAM), tparamu, List(), TypeBoundsTree(Ident(NothingClass), CompoundTypeTree(Template(List(Ident(BaseUniverseClass), Ident(SingletonClass)), emptyValDef, List()))))),
          List(List(ValDef(Modifiers(PARAM), nme.MIRROR_UNTYPED, AppliedTypeTree(Ident(MirrorOfClass), List(Ident(tparamu))), EmptyTree))),
          SelectFromTypeTree(Ident(tparamu), tpnme.Tree),
          Block(
            ValDef(NoMods, nme.UNIVERSE_SHORT, Ident(ApiUniverseClass), TypeApply(Select(Select(Ident(nme.MIRROR_UNTYPED), nme.universe), nme.asInstanceOf_), List(Ident(ApiUniverseClass)))),
            ValDef(NoMods, nme.MIRROR_SHORT, Select(Ident(nme.UNIVERSE_SHORT), tpnme.Mirror), TypeApply(Select(Ident(nme.MIRROR_UNTYPED), nme.asInstanceOf_), List(Select(Ident(nme.UNIVERSE_SHORT), tpnme.Mirror)))),
            TypeApply(Select(Apply(TypeApply(Ident(reifierName), List(SingletonTypeTree(Ident(nme.UNIVERSE_SHORT)))), List(Ident(nme.MIRROR_SHORT))), nme.asInstanceOf_), List(SelectFromTypeTree(Ident(tparamu), tpnme.Tree)))
          ))
      )
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
      emptyValDef,
      List(
        DefDef(NoMods, nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(()))))
        ) ++ reifierPreamble ++ List(
        DefDef(NoMods,
          reifierName,
          List(TypeDef(Modifiers(PARAM), tparamu, List(), TypeBoundsTree(Ident(NothingClass), CompoundTypeTree(Template(List(Ident(reifierUniverse), Ident(SingletonClass)), emptyValDef, List()))))),
          List(List(ValDef(Modifiers(PARAM), nme.MIRROR_UNTYPED, AppliedTypeTree(Ident(MirrorOfClass), List(Ident(tparamu))), EmptyTree))),
          reifierTpt, reifierBody))))
    Block(tpec, ApplyConstructor(Ident(tpec.name), List()))
  }

  private def mkWrapper(universe: Tree, mirror: Tree, wrappee: Tree): Tree = {
    val universeAlias = ValDef(NoMods, nme.UNIVERSE_SHORT, SingletonTypeTree(universe), universe)
    val mirrorAlias = ValDef(NoMods, nme.MIRROR_SHORT, Select(Ident(nme.UNIVERSE_SHORT), tpnme.Mirror), mirror orElse mkDefaultMirrorRef(global)(universe, typer))
    Block(List(universeAlias, mirrorAlias), wrappee)
  }

  object ReifiedTree {
    def apply(universe: Tree, mirror: Tree, symtab: SymbolTable, rtree: Tree, tpe: Type, rtpe: Tree, concrete: Boolean): Tree = {
      val tagFactory = if (concrete) nme.TypeTag else nme.AbsTypeTag
      val tagCtor = TypeApply(Select(Select(Ident(nme.UNIVERSE_SHORT), tagFactory), nme.apply), List(TypeTree(tpe)))
      val exprCtor = TypeApply(Select(Select(Ident(nme.UNIVERSE_SHORT), nme.Expr), nme.apply), List(TypeTree(tpe)))
      val tagArgs = List(Ident(nme.MIRROR_SHORT), mkCreator(tpnme.REIFY_TYPECREATOR_PREFIX, symtab, rtpe))
      val unwrapped = Apply(Apply(exprCtor, List(Ident(nme.MIRROR_SHORT), mkCreator(tpnme.REIFY_TREECREATOR_PREFIX, symtab, rtree))), List(Apply(tagCtor, tagArgs)))
      mkWrapper(universe, mirror, unwrapped)
    }

    def unapply(tree: Tree): Option[(Tree, Tree, SymbolTable, Tree, Type, Tree, Boolean)] = tree match {
      case Block(
        List(udef @ ValDef(_, _, _, universe), mdef @ ValDef(_, _, _, mirror)),
        Apply(
          Apply(TypeApply(_, List(ttpe @ TypeTree())), List(_, Block(List(ClassDef(_, _, _, Template(_, _, List(_, _, DefDef(_, _, _, _, _, Block(_ :: _ :: symbolTable1, rtree)))))), _))),
          // todo. doesn't take into account optimizations such as $u.TypeTag.Int or the upcoming closure optimization
          List(Apply(TypeApply(tagFactory @ Select(_, _), _), List(_, Block(List(ClassDef(_, _, _, Template(_, _, List(_, DefDef(_, _, _, _, _, Block(_ :: _ :: symbolTable2, rtpe)))))), _))))))
        if udef.name == nme.UNIVERSE_SHORT && mdef.name == nme.MIRROR_SHORT =>
          val tagFlavor = tagFactory match {
            case Select(Select(_, tagFlavor), _) => tagFlavor
            case Select(_, tagFlavor) => tagFlavor
          }
          Some(universe, mirror, SymbolTable(symbolTable1 ++ symbolTable2), rtree, ttpe.tpe, rtpe, tagFlavor == nme.TypeTag)
      case _ =>
        None
    }
  }

  object ReifiedType {
    def apply(universe: Tree, mirror: Tree, symtab: SymbolTable, tpe: Type, rtpe: Tree, concrete: Boolean) = {
      val tagFactory = if (concrete) nme.TypeTag else nme.AbsTypeTag
      val ctor = TypeApply(Select(Select(Ident(nme.UNIVERSE_SHORT), tagFactory), nme.apply), List(TypeTree(tpe)))
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
          Some(universe, mirror, SymbolTable(symtab), ttpe.tpe, rtpe, tagFlavor == nme.TypeTag)
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

  object FreeDef {
    def unapply(tree: Tree): Option[(Tree, TermName, Tree, Long, String)] = tree match {
      case FreeTermDef(uref, name, binding, flags, origin) =>
        Some(uref, name, binding, flags, origin)
      case FreeTypeDef(uref, name, binding, flags, origin) =>
        Some(uref, name, binding, flags, origin)
      case _ =>
        None
    }
  }

  object FreeTermDef {
    def unapply(tree: Tree): Option[(Tree, TermName, Tree, Long, String)] = tree match {
      case
        ValDef(_, name, _, Apply(
          Select(Select(uref1 @ Ident(_), build1), newFreeTerm),
          List(
            _,
            _,
            binding,
            Apply(Select(Select(uref2 @ Ident(_), build2), flagsFromBits), List(Literal(Constant(flags: Long)))),
            Literal(Constant(origin: String)))))
      if uref1.name == nme.UNIVERSE_SHORT && build1 == nme.build && newFreeTerm == nme.newFreeTerm &&
         uref2.name == nme.UNIVERSE_SHORT && build2 == nme.build && flagsFromBits == nme.flagsFromBits =>
        Some(uref1, name, binding, flags, origin)
      case _ =>
        None
    }
  }

  object FreeTypeDef {
    def unapply(tree: Tree): Option[(Tree, TermName, Tree, Long, String)] = tree match {
      case
        ValDef(_, name, _, Apply(
          Select(Select(uref1 @ Ident(_), build1), newFreeType),
          List(
            _,
            _,
            value,
            Apply(Select(Select(uref2 @ Ident(_), build2), flagsFromBits), List(Literal(Constant(flags: Long)))),
            Literal(Constant(origin: String)))))
      if uref1.name == nme.UNIVERSE_SHORT && build1 == nme.build && (newFreeType == nme.newFreeType || newFreeType == nme.newFreeExistential) &&
         uref2.name == nme.UNIVERSE_SHORT && build2 == nme.build && flagsFromBits == nme.flagsFromBits =>
        value match {
          case Apply(TypeApply(Select(Select(uref3 @ Ident(_), typeTag), apply), List(binding)), List(Literal(Constant(null)), _))
          if uref3.name == nme.UNIVERSE_SHORT && typeTag == nme.TypeTag && apply == nme.apply =>
            Some(uref1, name, binding, flags, origin)
          case Apply(TypeApply(Select(uref3 @ Ident(_), typeTag), List(binding)), List(Literal(Constant(null)), _))
          if uref3.name == nme.UNIVERSE_SHORT && typeTag == nme.TypeTag =>
            Some(uref1, name, binding, flags, origin)
          case _ =>
            throw new Error("unsupported free type def: %s%n%s".format(value, showRaw(value)))
        }
      case _ =>
        None
    }
  }

  object FreeRef {
    def unapply(tree: Tree): Option[(Tree, TermName)] = tree match {
      case Apply(Select(Select(uref @ Ident(_), build), ident), List(Ident(name: TermName)))
      if build == nme.build && ident == nme.Ident && name.startsWith(nme.REIFY_FREE_PREFIX) =>
        Some(uref, name)
      case _ =>
        None
    }
  }

  object SymDef {
    def unapply(tree: Tree): Option[(Tree, TermName, Long, Boolean)] = tree match {
      case
        ValDef(_, name, _, Apply(
          Select(Select(uref1 @ Ident(_), build1), newNestedSymbol),
          List(
            _,
            _,
            _,
            Apply(Select(Select(uref2 @ Ident(_), build2), flagsFromBits), List(Literal(Constant(flags: Long)))),
            Literal(Constant(isClass: Boolean)))))
      if uref1.name == nme.UNIVERSE_SHORT && build1 == nme.build && newNestedSymbol == nme.newNestedSymbol &&
         uref2.name == nme.UNIVERSE_SHORT && build2 == nme.build && flagsFromBits == nme.flagsFromBits =>
        Some(uref1, name, flags, isClass)
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
      case Ident(name) if name.isTermName =>
        Some(tree)
      case This(_) =>
        Some(tree)
      case _ =>
        None
    }
  }

  object BoundType {
    def unapply(tree: Tree): Option[Tree] = tree match {
      case Select(_, name) if name.isTypeName =>
        Some(tree)
      case SelectFromTypeTree(_, name) if name.isTypeName =>
        Some(tree)
      case Ident(name) if name.isTypeName =>
        Some(tree)
      case _ =>
        None
    }
  }
}
