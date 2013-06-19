import org.scalacheck._
import Prop._
import Gen._
import Arbitrary._

import scala.reflect.runtime.universe._
import Flag._

object TermConstructionProps extends QuasiquoteProperties("term construction") {
  val anyRef = Select(Ident(TermName("scala")), TypeName("AnyRef"))
  val emtpyConstructor =
    DefDef(
      Modifiers(), nme.CONSTRUCTOR, List(),
      List(List()), TypeTree(), Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(()))))

  def classWithMethods(name: TypeName, methods: List[DefDef] = Nil) =
    ClassDef(
      Modifiers(),  name, List(),
      Template(List(anyRef), emptyValDef, List(emtpyConstructor) ++ methods))

  property("splice single tree return tree itself") = forAll { (t: Tree) =>
    q"$t" ≈ t
  }

  property("splice trees into if expression") = forAll { (t1: Tree, t2: Tree, t3: Tree) =>
    q"if($t1) $t2 else $t3" ≈ If(t1, t2, t3)
  }

  property("splice term name into val") = forAll { (name: TermName) =>
    q"val $name = 0" ≈ ValDef(Modifiers(), name, TypeTree(), Literal(Constant(0)))
  }

  property("splice type name into typedef") = forAll { (name1: TypeName, name2: TypeName) =>
    q"type $name1 = $name2" ≈ TypeDef(Modifiers(), name1, List(), Ident(name2))
  }

  property("splice term name into class") = forAll { (name: TypeName) =>
    q"class $name" ≈ classWithMethods(name)
  }

  property("splice method into class") = forAll { (name: TypeName, method: DefDef) =>
    q"class $name { $method }" ≈ classWithMethods(name, List(method))
  }

  property("splice trees into ascriptiopn") = forAll { (t1: Tree, t2: Tree) =>
    q"$t1 : $t2" ≈ Typed(t1, t2)
  }

  property("splice trees into apply") = forAll { (t1: Tree, t2: Tree, t3: Tree) =>
    q"$t1($t2, $t3)" ≈ Apply(t1, List(t2, t3))
  }

  property("splice trees with .. cardinality into apply") = forAll { (ts: List[Tree]) =>
    q"f(..$ts)" ≈ Apply(q"f", ts)
  }

  property("splice iterable into apply") = forAll { (trees: List[Tree]) =>
    val itrees: Iterable[Tree] = trees
    q"f(..$itrees)" ≈ Apply(q"f", trees)
  }

  property("splice trees with ... cardinality into apply") = forAll { (ts1: List[Tree], ts2: List[Tree]) =>
    val argss = List(ts1, ts2)
    q"f(...$argss)" ≈ Apply(Apply(q"f", ts1), ts2)
  }

  property("splice term name into assign") = forAll { (name: TermName, t: Tree) =>
    q"$name = $t" ≈ Assign(Ident(name), t)
  }

  property("splice trees into block") = forAll { (t1: Tree, t2: Tree, t3: Tree) =>
    q"""{
      $t1
      $t2
      $t3
    }""" ≈ Block(List(t1, t2), t3)
  }

  property("splice type name into class parents") = forAll { (name: TypeName, parent: TypeName) =>
    q"class $name extends $parent" ≈
      ClassDef(
        Modifiers(),  name, List(),
        Template(List(Ident(parent)), emptyValDef, List(emtpyConstructor)))
  }

  property("splice tree into new") = forAll { (tree: Tree) =>
    q"new $tree" ≈ Apply(Select(New(tree), nme.CONSTRUCTOR), List())
  }

  property("splice tree into return") = forAll { (tree: Tree) =>
    q"return $tree" ≈ Return(tree)
  }

  property("splice a list of arguments") = forAll { (fun: Tree, args: List[Tree]) =>
    q"$fun(..$args)" ≈ Apply(fun, args)
  }

  property("splice list and non-list fun arguments") = forAll { (fun: Tree, arg1: Tree, arg2: Tree, args: List[Tree]) =>
    q"$fun(..$args, $arg1, $arg2)" ≈ Apply(fun, args ++ List(arg1) ++ List(arg2)) &&
    q"$fun($arg1, ..$args, $arg2)" ≈ Apply(fun, List(arg1) ++ args ++ List(arg2)) &&
    q"$fun($arg1, $arg2, ..$args)" ≈ Apply(fun, List(arg1) ++ List(arg2) ++ args)
  }

  property("splice members into class") = forAll { (name: TypeName, defs: List[DefDef], extra: DefDef) =>
    q"""class $name {
      ..$defs
      $extra
    }""" ≈ classWithMethods(name, defs ++ List(extra))
  }

  property("splice into new") = forAll { (name: TypeName, body: List[Tree]) =>
    q"new $name { ..$body }" ≈
      q"""{
        final class $$anon extends $name {
          ..$body
        }
        new $$anon
      }"""
  }


  property("splice tree into singleton type tree") = forAll { (name: TypeName, t: Tree) =>
    q"type $name = $t.type" ≈ q"type $name = ${SingletonTypeTree(t)}"
  }

  property("splice type name into this") = forAll { (T: TypeName) =>
    q"$T.this" ≈ This(T)
  }

  property("splice tree into throw") = forAll { (t: Tree) =>
    q"throw $t" ≈ Throw(t)
  }

  property("splice trees into type apply") = forAll { (fun: TreeIsTerm, types: List[Tree]) =>
    q"$fun[..$types]" ≈ TypeApply(fun, types)
  }

  property("splice type names into type bounds") = forAll { (T1: TypeName, T2: TypeName, T3: TypeName) =>
    q"type $T1 >: $T2 <: $T3" ≈
      TypeDef(
        Modifiers(DEFERRED), T1, List(),
        TypeBoundsTree(Ident(T2), Ident(T3)))
  }

  property("splice trees names into type bounds") = forAll { (T: TypeName, t1: Tree, t2: Tree) =>
    q"type $T >: $t1 <: $t2" ≈
      TypeDef(
        Modifiers(DEFERRED), T, List(),
        TypeBoundsTree(t1, t2))
  }

  property("splice tparams into typedef (1)") = forAll { (T: TypeName, targs: List[TypeDef], t: Tree) =>
    q"type $T[..$targs] = $t" ≈ TypeDef(Modifiers(), T, targs, t)
  }

  property("splice tparams into typedef (2)") = forAll { (T: TypeName, targs1: List[TypeDef], targs2: List[TypeDef], t: Tree) =>
    q"type $T[..$targs1, ..$targs2] = $t" ≈ TypeDef(Modifiers(), T, targs1 ++ targs2, t)
  }

  property("splice tparams into typedef (3)") = forAll { (T: TypeName, targ: TypeDef, targs: List[TypeDef], t: Tree) =>
    q"type $T[$targ, ..$targs] = $t" ≈ TypeDef(Modifiers(), T, targ :: targs, t)
  }

  property("splice typename into typedef with default bounds") = forAll { (T1: TypeName, T2: TypeName, t: Tree) =>
    q"type $T1[$T2 >: Any <: Nothing] = $t" ≈
      TypeDef(
        Modifiers(), T1,
        List(TypeDef(
          Modifiers(PARAM), T2,
          List(),
          TypeBoundsTree(
            Ident(TypeName("Any")),
            Ident(TypeName("Nothing"))))),
        t)
  }

  property("splice type names into compound type tree") = forAll { (T: TypeName, A: TypeName, B: TypeName) =>
    q"type $T = $A with $B" ≈
      TypeDef(
        Modifiers(), T, List(),
        CompoundTypeTree(
          Template(List(Ident(A), Ident(B)), ValDef(Modifiers(PRIVATE), nme.WILDCARD, TypeTree(), EmptyTree), List())))
  }

  property("splice trees into existential type tree") = forAll {
    (T1: TypeName, T2: TypeName, X: TypeName, Lo: TypeName, Hi: TypeName) =>

    q"type $T1 = $T2[$X] forSome { type $X >: $Lo <: $Hi }" ≈
      TypeDef(
        Modifiers(), T1, List(),
        ExistentialTypeTree(
          AppliedTypeTree(Ident(T2), List(Ident(X))),
          List(
            TypeDef(Modifiers(DEFERRED), X, List(), TypeBoundsTree(Ident(Lo), Ident(Hi))))))
  }

  property("splice names into import selector") = forAll {
    (expr: Tree, plain: Name, oldname: Name, newname: Name, discard: Name) =>

    val Import(expr1, List(
      ImportSelector(plain11, _, plain12, _),
      ImportSelector(oldname1, _, newname1, _),
      ImportSelector(discard1, _, wildcard, _))) =
        q"import $expr.{$plain, $oldname => $newname, $discard => _}"

    expr1 ≈ expr && plain11 == plain12 && plain12 == plain &&
    oldname1 == oldname && newname1 == newname && discard1 == discard && wildcard == nme.WILDCARD
  }

  property("splice trees into while loop") = forAll { (cond: Tree, body: Tree) =>
    val LabelDef(_, List(), If(cond1, Block(List(body1), Apply(_, List())), Literal(Constant(())))) = q"while($cond) $body"
    body1 ≈ body && cond1 ≈ cond
  }

  property("splice trees into do while loop") = forAll { (cond: Tree, body: Tree) =>
    val LabelDef(_, List(), Block(List(body1), If(cond1, Apply(_, List()), Literal(Constant(()))))) = q"do $body while($cond)"
    body1 ≈ body && cond1 ≈ cond
  }

  property("splice trees into alternative") = forAll { (c: Tree, A: Tree, B: Tree) =>
    q"$c match { case $A | $B => }" ≈
      Match(c, List(
        CaseDef(Alternative(List(A, B)), EmptyTree, Literal(Constant(())))))
  }

  property("splice into applied type tree") = forAll { (T1: TypeName, T2: TypeName, args: List[Tree]) =>
    q"type $T1 = $T2[..$args]" ≈
      TypeDef(
        Modifiers(), T1, List(),
        AppliedTypeTree(Ident(T2), args))
  }

  property("splice list of trees into block (1)") = forAll { (trees: List[Tree]) =>
    q"{ ..$trees }" ≈ (trees match {
      case Nil => Block(Nil, q"()")
      case _   => Block(trees.init, trees.last)
    })
  }

  property("splice list of trees into block (2)") = forAll { (trees1: List[Tree], trees2: List[Tree]) =>
    q"{ ..$trees1 ; ..$trees2 }" ≈ ((trees1 ++ trees2) match {
      case Nil   => Block(Nil, Literal(Constant(())))
      case trees => Block(trees.init, trees.last)
    })
  }

  property("splice list of trees into block (3)") = forAll { (trees: List[Tree], tree: Tree) =>
    q"{ ..$trees; $tree }" ≈ Block(trees, tree)
  }

  // TODO: this test needs to be implemented
  // property("splice valdef into class param") = forAll { (name: TypeName, valdef: ValDef) =>
  //   q"class $name($valdef)" ≈ ...
  // }

  // TODO: this test needs to be implemented
  // property("splice tree into super") = forAll { (T: TypeName, t: Tree) =>
  //   q"$t.super[$T]" ≈ ...
  // }

  // TODO: this test needs to be implemented
  // property("splice targs into classdef") = forAll { (C: TypeName, targs: List[TypeDef], t: Tree) =>
  //   q"class $C[..$targs]" ≈ ...
  // }

  def assertSameAnnots(tree: {def mods: Modifiers}, annots: List[Tree]) =
    assert(tree.mods.annotations ≈ annots,
           s"${tree.mods.annotations} =/= ${annots}")

  def assertSameAnnots(tree1: {def mods: Modifiers}, tree2: {def mods: Modifiers}) =
    assert(tree1.mods.annotations ≈ tree2.mods.annotations,
           s"${tree1.mods.annotations} =/= ${tree2.mods.annotations}")

  property("splice type name into annotation") = test {
    val name = TypeName("annot")
    assertSameAnnots(q"@$name def foo", List(annot(name)))
  }

  property("splice ident into annotation") = test {
    val name = TypeName("annot")
    val ident = Ident(name)
    assertSameAnnots(q"@$ident def foo", List(annot(name)))
  }

  property("splice idents into annotation") = test {
    val idents = List(Ident(TypeName("annot1")), Ident(TypeName("annot2")))
    assertSameAnnots(q"@..$idents def foo",
      idents.map { ident => Apply(Select(New(ident), nme.CONSTRUCTOR), List()) })
  }

  property("splice constructor calls into annotation") = test {
    val ctorcalls = List(annot("a1"), annot("a2"))
    assertSameAnnots(q"@..$ctorcalls def foo", ctorcalls)
  }

  property("splice multiple annotations (1)") = test {
    val annot1 = annot("a1")
    val annot2 = annot("a2")
    val res = q"@$annot1 @$annot2 def foo"
    assertSameAnnots(res, List(annot1, annot2))
  }

  property("splice multiple annotations (2)") = test {
    val annot1 = annot("a1")
    val annots = List(annot("a2"), annot("a3"))
    val res = q"@$annot1 @..$annots def foo"
    assertSameAnnots(res, annot1 :: annots)
  }

  property("splice annotations with arguments (1)") = test {
    val a = annot("a", List(q"x"))
    assertSameAnnots(q"@$a def foo", q"@a(x) def foo")
  }

  property("splice annotations with arguments (2)") = test {
    val a = newTypeName("a")
    assertSameAnnots(q"@$a(x) def foo", q"@a(x) def foo")
  }

  property("splice annotations with arguments (3") = test {
    val a = Ident(newTypeName("a"))
    assertSameAnnots(q"@$a(x) def foo", q"@a(x) def foo")
  }

  property("can't splice annotations with arguments specificed twice") = test {
    val a = annot("a", List(q"x"))
    assertThrows[IllegalArgumentException] {
      q"@$a(y) def foo"
    }
  }

  property("splice term into brackets") = test {
    val a = q"a"
    assert(q"($a)" ≈ a)
  }

  property("splice terms into tuple") = test {
    val a1 = q"a1"
    val a2 = q"a2"
    val as = List(a1, a2)
    assert(q"(..$as)" ≈ q"Tuple2($a1, $a2)")
    assert(q"(a0, ..$as)" ≈ q"Tuple3(a0, $a1, $a2)")
  }

  property("splcie empty list into tuple") = test {
    val empty = List[Tree]()
    assert(q"(..$empty)" ≈ q"()")
  }
}
