import org.scalacheck._
import Prop._
import Gen._
import Arbitrary._

import scala.reflect.runtime.universe._
import Flag._

object DefinitionConstructionProps
  extends QuasiquoteProperties("definition construction")
  with ClassConstruction
  with TraitConstruction
  with TypeDefConstruction
  with ValDefConstruction

trait ClassConstruction { self: QuasiquoteProperties =>
  val anyRef = Select(Ident(TermName("scala")), TypeName("AnyRef"))
  val emtpyConstructor =
    DefDef(Modifiers(), nme.CONSTRUCTOR, List(),
      List(List()), TypeTree(), Block(List(pendingSuperCall), Literal(Constant(()))))
  def classWith(name: TypeName, parents: List[Tree] = List(anyRef), body: List[DefDef] = Nil) =
    ClassDef(
      Modifiers(), name, List(),
      Template(parents, emptyValDef, emtpyConstructor :: body))

  property("splice term name into class") = forAll { (name: TypeName) =>
    eqAst(q"class $name", "class " + name.toString)
  }

  property("splice method into class") = forAll { (name: TypeName, method: DefDef) =>
    q"class $name { $method }" ≈ classWith(name, body = List(method))
  }

  property("splice members into class") = forAll { (name: TypeName, defs: List[DefDef], extra: DefDef) =>
    q"""class $name {
      ..$defs
      $extra
    }""" ≈ classWith(name, body = defs :+ extra)
  }

  property("splice type name into class parents") = forAll { (name: TypeName, parent: TypeName) =>
    q"class $name extends $parent" ≈ classWith(name, parents = List(Ident(parent)))
  }
}

trait TraitConstruction { self: QuasiquoteProperties =>

}

trait TypeDefConstruction { self: QuasiquoteProperties =>
  property("splice type name into typedef") = forAll { (name1: TypeName, name2: TypeName) =>
    q"type $name1 = $name2" ≈ TypeDef(Modifiers(), name1, List(), Ident(name2))
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

  property("splice tree into singleton type tree") = forAll { (name: TypeName, t: Tree) =>
    q"type $name = $t.type" ≈ q"type $name = ${SingletonTypeTree(t)}"
  }

  property("splice into applied type tree") = forAll { (T1: TypeName, T2: TypeName, args: List[Tree]) =>
    q"type $T1 = $T2[..$args]" ≈
      TypeDef(Modifiers(), T1, List(),
        if(args.nonEmpty) AppliedTypeTree(Ident(T2), args) else Ident(T2))
  }
}

trait ValDefConstruction { self: QuasiquoteProperties =>
  property("splice term name into val") = forAll { (name: TermName, tpt: Tree, rhs: Tree) =>
    q"val $name: $tpt = $rhs" ≈ ValDef(Modifiers(), name, tpt, rhs)
  }

  property("splice term name into var") = forAll { (name: TermName, tpt: Tree, rhs: Tree) =>
    q"var $name: $tpt = $rhs" ≈ ValDef(Modifiers(MUTABLE), name, tpt, rhs)
  }
}

trait MethodConstruction { self: QuasiquoteProperties =>
  property("splice paramss into defdef") = test {
    val paramss = List(q"val x: Int") :: List(q"val y: Int = 1") :: Nil
    assert(q"def foo(...$paramss)" ≈ parse("def foo(x: Int)(y: Int = 1)"))
  }

  property("splice tparams into defdef") = test {
    val tparams = q"type A" :: q"type B <: Bippy" :: Nil
    assert(q"def foo[..$tparams]" ≈ parse("def foo[A, B <: Bippy]"))
  }

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

  property("splice improper tree into annot") = test {
    val t = tq"Foo[Baz]"
    assertThrows[IllegalArgumentException] {
      q"@$t def foo"
    }
  }

  property("can't splice annotations with arguments specificed twice") = test {
    val a = annot("a", List(q"x"))
    assertThrows[IllegalArgumentException] {
      q"@$a(y) def foo"
    }
  }
}