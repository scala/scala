import org.scalacheck._
import Prop._
import Gen._
import Arbitrary._

import scala.reflect.runtime.universe._
import Flag._

object TermConstructionProps extends Properties("construction")
                                with TreeSimiliarity
                                with ArbitraryTreesAndNames {

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

  // TODO: this test needs to be fixed
  // property("splice list of trees into block") = forAll { (trees: List[Tree]) =>
  //   q"{ ..$trees }" == Block(trees.init, trees.last)
  // }

  property("splice list of trees into block with unit") = forAll { (trees: List[Tree]) =>
    q"{ ..$trees; () }" ≈ Block(trees, q"()")
  }

  // TODO: this test needs to be fixed
  // property("splice valdef into class param") = forAll { (name: TypeName, valdef: ValDef) =>
  //   q"class $name($valdef)" ≈ ellipsis
  // }

  property("splice tree into singleton type tree") = forAll { (name: TypeName, t: Tree) =>
    q"type $name = $t.type" ≈ q"type $name = ${SingletonTypeTree(t)}"
  }

// TODO: this test needs to be fixed
  // property("splice tree into super") = forAll { (T: TypeName, t: Tree) =>
  //   q"$t.super[$T]" ≈ Super(This(t), T)
  // }

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
      TypeDef(Modifiers(), T1, List(TypeDef(ellipsis, T2, List(), TypeBoundsTree(ellipsis, ellipsis))), t)
  }

  // TODO: this test needs to be fixed
  // property("splice targs into classdef") = forAll { (C: TypeName, targs: List[TypeDef], t: Tree) =>
  //   q"class $C[..$targs]" ≈ ellipsis
  // }

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
    q"import $expr.{$plain, $oldname => $newname, $discard => _}" ≈
      Import(expr,
        List(
          ImportSelector(plain, 0, plain, 0),
          ImportSelector(oldname, 0, newname, 0),
          ImportSelector(discard, 0, nme.WILDCARD, 0)))
  }

  property("splice trees into while loop") = forAll { (cond: Tree, body: Tree) =>
    q"while($cond) $body" ≈
      LabelDef(
        ellipsis,
        List(),
        If(cond,
          Block(List(body), Apply(ellipsis, List())),
          Literal(Constant(()))))
  }

  property("splice trees into do while loop") = forAll { (cond: Tree, body: Tree) =>
    q"do $body while($cond)" ≈
      LabelDef(
        ellipsis,
        List(),
        Block(
          List(body),
          If(cond,
            Apply(ellipsis, List()),
            Literal(Constant(())))))
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

  // --- NEGATIVE TESTS TEMPLATES

  // // not allowed, T2 should be a name
  // property("splice typename into typedef with default bounds") = forAll { (T1: TypeName, T2: TypeDef, t: Tree) =>
  //   q"type $T1[$T2 >: _root_.scala.Any <: _root_.scala.Nothing] = $t" ≈
  //     TypeDef(Modifiers(), T1, List(T2), t)
  // }

}

