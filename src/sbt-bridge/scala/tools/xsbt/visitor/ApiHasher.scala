/*
 * Zinc - The incremental compiler for Scala.
 * Copyright Scala Center, Lightbend dba Akka, and Mark Harrah
 *
 * Scala (https://www.scala-lang.org)
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools
package xsbt
package visitor

import scala.reflect.ClassTag
import scala.util.hashing.MurmurHash3
import xsbti.UseScope
import xsbti.api.{ Access, DefinitionType, IdQualifier, Lazy, Modifiers, NameHash, ParameterModifier, Private, Protected, Public, ThisQualifier, Unqualified, Variance }

import ApiHasher._

/**
 * Computes the hashes Zinc computes from a class's `xsbti.api` tree (`HashAPI`, `NameHashing`)
 * without building the tree.
 *
 * `HashAPI` is a running fold: each node `extend`s one hash with a fixed sequence of ints, so a
 * node's contribution depends on what came before it and can't be kept as a single int. A node
 * becomes a `Tape` instead: the ints `HashAPI` would `extend` with, and the structures embedded
 * among them. Structures stay as nodes because `HashAPI` keeps per-hash state about them (it
 * hashes a structure met a second time by the hash at the end of its first visit), and because
 * `NameHashing` collects the members of every structure it can reach.
 */
final class ApiHasher extends ApiVisitor {
  type Type = Tape
  type Structure = Tape
  type PathComponent = Tape
  type Annotation = Tape
  type AnnotationArgument = Tape
  type TypeParameter = Tape
  type MethodParameter = Tape
  type ParameterList = Tape
  type ClassDefinition = HDef
  type ClassLikeDef = HDef
  type ClassLike = HClass

  implicit val typeTag: ClassTag[Tape] = ClassTag(classOf[Tape])
  implicit def pathComponentTag: ClassTag[Tape] = typeTag
  implicit def annotationTag: ClassTag[Tape] = typeTag
  implicit def annotationArgumentTag: ClassTag[Tape] = typeTag
  implicit def typeParameterTag: ClassTag[Tape] = typeTag
  implicit def methodParameterTag: ClassTag[Tape] = typeTag
  implicit def parameterListTag: ClassTag[Tape] = typeTag
  implicit val classDefinitionTag: ClassTag[HDef] = ClassTag(classOf[HDef])

  private[this] val b = new TapeBuilder

  private[this] val theEmptyType = Tape.of(EmptyTypeHash)
  private[this] val theThis = Tape.of(ThisPathHash)

  def emptyType: Tape = theEmptyType
  def parameterRef(id: String): Tape = Tape.of(ParameterRefHash, str(id))
  def id(name: String): Tape = Tape.of(IdPathHash, str(name))
  def thisComponent: Tape = theThis
  def singleton(path: Array[Tape]): Tape = { b.int(SingletonHash); b.tapes(path); b.result() }
  def projection(prefix: Tape, id: String): Tape = { b.int(ProjectionHash); b.str(id); b.tape(prefix); b.result() }
  def parameterized(base: Tape, args: Array[Tape]): Tape = { b.int(ParameterizedHash); b.tape(base); b.tapes(args); b.result() }
  def annotated(base: Tape, annotations: Array[Tape]): Tape = { b.int(AnnotatedHash); b.tape(base); b.tapes(annotations); b.result() }
  def constant(base: Tape, value: String): Tape = { b.int(ConstantHash); b.str(value); b.tape(base); b.result() }
  def existential(base: Tape, clause: Array[Tape]): Tape = { b.int(ExistentialHash); b.tapes(clause); b.tape(base); b.result() }
  def polymorphic(base: Tape, parameters: Array[Tape]): Tape = { b.int(PolymorphicHash); b.tapes(parameters); b.tape(base); b.result() }
  def structure(parents: Lazy[Array[Tape]], declared: Lazy[Array[HDef]], inherited: Lazy[Array[HDef]]): Tape =
    Tape.of(new HStructure(parents, declared, inherited))

  def annotationArgument(name: String, value: String): Tape = Tape.of(str(name), str(value))
  def annotation(base: Tape, arguments: Array[Tape]): Tape = { b.tape(base); b.tapes(arguments); b.result() }
  def typeParameter(
      id: String,
      annotations: Array[Tape],
      typeParameters: Array[Tape],
      variance: Variance,
      lowerBound: Tape,
      upperBound: Tape
  ): Tape = {
    b.str(id); b.int(variance.ordinal); b.tapes(typeParameters); b.tape(lowerBound); b.tape(upperBound); b.tapes(annotations)
    b.result()
  }
  def methodParameter(name: String, tpe: Tape, hasDefault: Boolean, modifier: ParameterModifier): Tape = {
    b.str(name); b.tape(tpe); b.int(modifier.ordinal); b.bool(hasDefault)
    b.result()
  }
  def parameterList(parameters: Array[Tape], isImplicit: Boolean): Tape = {
    b.int(ValueParamsHash); b.bool(isImplicit); b.tapes(parameters)
    b.result()
  }

  private def header(name: String, access: Access, modifiers: Modifiers, annotations: Array[Tape]): Unit = {
    b.str(name); b.tapes(annotations); b.int(modifiers.raw.toInt); hashAccess(access)
  }
  private def hashAccess(a: Access): Unit = a match {
    case _: Public => b.int(PublicHash)
    case p: Protected => b.int(ProtectedHash); hashQualifier(p.qualifier)
    case p: Private => b.int(PrivateHash); hashQualifier(p.qualifier)
    case x => throw new MatchError(x)
  }
  private def hashQualifier(q: xsbti.api.Qualifier): Unit = q match {
    case _: Unqualified => b.int(UnqualifiedHash)
    case _: ThisQualifier => b.int(ThisQualifierHash)
    case id: IdQualifier => b.int(IdQualifierHash); b.str(id.value)
    case x => throw new MatchError(x)
  }

  def defDef(
      name: String,
      access: Access,
      modifiers: Modifiers,
      annotations: Array[Tape],
      typeParameters: Array[Tape],
      valueParameters: Array[Tape],
      returnType: Tape
  ): HDef = {
    header(name, access, modifiers, annotations)
    b.int(DefHash); b.tapes(typeParameters); b.tapes(valueParameters); b.tape(returnType)
    new HDef(name, access, modifiers, if (modifiers.isSuperAccessor) KindTraitBreaker else KindOther, b.result())
  }
  def valDef(name: String, access: Access, modifiers: Modifiers, annotations: Array[Tape], tpe: Tape): HDef = {
    header(name, access, modifiers, annotations); b.int(ValHash); b.tape(tpe)
    new HDef(name, access, modifiers, KindTraitBreaker, b.result())
  }
  def varDef(name: String, access: Access, modifiers: Modifiers, annotations: Array[Tape], tpe: Tape): HDef = {
    header(name, access, modifiers, annotations); b.int(VarHash); b.tape(tpe)
    new HDef(name, access, modifiers, KindTraitBreaker, b.result())
  }
  def typeAlias(
      name: String,
      access: Access,
      modifiers: Modifiers,
      annotations: Array[Tape],
      typeParameters: Array[Tape],
      tpe: Tape
  ): HDef = {
    header(name, access, modifiers, annotations); b.int(TypeAliasHash); b.tapes(typeParameters); b.tape(tpe)
    new HDef(name, access, modifiers, KindOther, b.result())
  }
  def typeDeclaration(
      name: String,
      access: Access,
      modifiers: Modifiers,
      annotations: Array[Tape],
      typeParameters: Array[Tape],
      lowerBound: Tape,
      upperBound: Tape
  ): HDef = {
    header(name, access, modifiers, annotations)
    b.int(TypeDeclHash); b.tapes(typeParameters); b.tape(lowerBound); b.tape(upperBound)
    new HDef(name, access, modifiers, KindOther, b.result())
  }
  def classLikeDef(
      name: String,
      access: Access,
      modifiers: Modifiers,
      annotations: Array[Tape],
      typeParameters: Array[Tape],
      definitionType: DefinitionType
  ): HDef = {
    header(name, access, modifiers, annotations); b.int(ClassDefHash); b.tapes(typeParameters)
    val kind = if (definitionType == DefinitionType.Module) KindTraitBreaker else KindOther
    new HDef(name, access, modifiers, kind, b.result())
  }
  def classLike(
      name: String,
      access: Access,
      modifiers: Modifiers,
      annotations: Array[Tape],
      definitionType: DefinitionType,
      selfType: Lazy[Tape],
      structure: Lazy[Tape],
      savedAnnotations: Array[String],
      childrenOfSealedClass: Array[Tape],
      topLevel: Boolean,
      typeParameters: Array[Tape]
  ): HClass = {
    header(name, access, modifiers, annotations)
    val h = b.result()
    b.tapes(typeParameters)
    new HClass(name, modifiers, definitionType, h, b.result(), selfType, structure, childrenOfSealedClass)
  }

  private[this] val strHashes = new java.util.HashMap[String, Integer]()
  private def str(s: String): Int = {
    val cached = strHashes.get(s)
    if (cached ne null) cached.intValue
    else {
      val h = MurmurHash3.stringHash(s)
      strHashes.put(s, h)
      h
    }
  }

  /** Accumulates one tape at a time: every node is built from finished children. */
  private final class TapeBuilder {
    private[this] var ints = new Array[Int](64)
    private[this] var n = 0
    private[this] var structs: Array[HStructure] = null
    private[this] var at: Array[Int] = null
    private[this] var ns = 0

    def int(i: Int): Unit = {
      if (n == ints.length) ints = java.util.Arrays.copyOf(ints, n * 2)
      ints(n) = i
      n += 1
    }
    def str(s: String): Unit = int(ApiHasher.this.str(s))
    def bool(x: Boolean): Unit = int(if (x) TrueHash else FalseHash)
    private def struct(s: HStructure, pos: Int): Unit = {
      if (structs eq null) { structs = new Array[HStructure](4); at = new Array[Int](4) }
      else if (ns == structs.length) {
        structs = java.util.Arrays.copyOf(structs, ns * 2); at = java.util.Arrays.copyOf(at, ns * 2)
      }
      structs(ns) = s
      at(ns) = pos
      ns += 1
    }
    def tape(t: Tape): Unit = {
      val base = n
      var i = 0
      while (i < t.structCount) { struct(t.structs(i), base + t.at(i)); i += 1 }
      i = 0
      val ts = t.ints
      while (i < ts.length) { int(ts(i)); i += 1 }
    }
    def tapes(ts: Array[Tape]): Unit = {
      int(ts.length)
      var i = 0
      while (i < ts.length) { tape(ts(i)); i += 1 }
    }
    def result(): Tape = {
      val t =
        if (ns == 0) new Tape(java.util.Arrays.copyOf(ints, n), Tape.NoStructs, Tape.NoAt)
        else new Tape(java.util.Arrays.copyOf(ints, n), java.util.Arrays.copyOf(structs, ns), java.util.Arrays.copyOf(at, ns))
      n = 0
      ns = 0
      t
    }
  }
}

object ApiHasher {
  final val ValHash = 1
  final val VarHash = 2
  final val DefHash = 3
  final val ClassDefHash = 4
  final val TypeDeclHash = 5
  final val TypeAliasHash = 6
  final val PublicHash = 30
  final val ProtectedHash = 31
  final val PrivateHash = 32
  final val UnqualifiedHash = 33
  final val ThisQualifierHash = 34
  final val IdQualifierHash = 35
  final val IdPathHash = 20
  final val ThisPathHash = 22
  final val ValueParamsHash = 40
  final val EmptyTypeHash = 51
  final val ParameterRefHash = 52
  final val SingletonHash = 53
  final val ProjectionHash = 54
  final val ParameterizedHash = 55
  final val AnnotatedHash = 56
  final val PolymorphicHash = 57
  final val ConstantHash = 58
  final val ExistentialHash = 59
  final val StructureHash = 60
  final val ClassHash = 70
  final val TrueHash = 97
  final val FalseHash = 98

  final val KindOther = 0
  /** `HashAPI`'s trait breakers: fields, objects and super accessors. */
  final val KindTraitBreaker = 1

  /** The ints `HashAPI` extends its hash with for one node, with structure `structs(i)` before `ints(at(i))`. */
  final class Tape(val ints: Array[Int], val structs: Array[HStructure], val at: Array[Int]) {
    def structCount: Int = structs.length
    /** A structure on its own, which `HashAPI` hashes without its members in some positions. */
    def bareStructure: HStructure = if (ints.length == 0 && structs.length == 1) structs(0) else null

    /**
     * Equal when the `xsbti.api` nodes would be: `ExtractAPI` merges a field's and its accessors'
     * annotations in a set, relying on that equality, which compares structures by identity.
     */
    override def equals(o: Any): Boolean = o match {
      case t: Tape =>
        java.util.Arrays.equals(ints, t.ints) && java.util.Arrays.equals(at, t.at) &&
          java.util.Arrays.equals(structs.asInstanceOf[Array[AnyRef]], t.structs.asInstanceOf[Array[AnyRef]])
      case _ => false
    }
    override def hashCode: Int = java.util.Arrays.hashCode(ints)
  }
  object Tape {
    val NoStructs = new Array[HStructure](0)
    val NoAt = new Array[Int](0)
    def of(is: Int*): Tape = new Tape(is.toArray, NoStructs, NoAt)
    def of(s: HStructure): Tape = new Tape(new Array[Int](0), Array(s), Array(0))
  }

  final class HStructure(val parents: Lazy[Array[Tape]], val declared: Lazy[Array[HDef]], val inherited: Lazy[Array[HDef]])

  /** A definition `NameHashing` collects: a member, or the class itself. */
  sealed abstract class HDefinition {
    def name: String
    def modifiers: Modifiers
  }

  final class HDef(val name: String, val access: Access, val modifiers: Modifiers, val kind: Int, val tape: Tape) extends HDefinition {
    def isNonPrivate: Boolean = access match {
      case p: Private => p.qualifier.isInstanceOf[IdQualifier]
      case _          => true
    }
    def isPrivate: Boolean = access.isInstanceOf[Private]
  }

  final class HClass(
      val name: String,
      val modifiers: Modifiers,
      val definitionType: DefinitionType,
      val header: Tape,
      val typeParameters: Tape,
      val selfType: Lazy[Tape],
      val structure: Lazy[Tape],
      val childrenOfSealedClass: Array[Tape]
  ) extends HDefinition

  /** What Zinc computes from a class's API in `AnalysisCallback.api`. */
  final class ClassHashes(val name: String, val definitionType: DefinitionType, val apiHash: Int, val extraHash: Int, val nameHashes: Array[NameHash])

  def hashes(c: HClass, optimizedSealed: Boolean): ClassHashes = {
    val apiHash = Fold.hashAPI(c, includeTraitBreakers = false)
    val extraHash =
      if (c.definitionType == DefinitionType.Trait) Fold.hashAPI(c, includeTraitBreakers = true) else apiHash
    new ClassHashes(c.name, c.definitionType, apiHash, extraHash, nameHashes(c, optimizedSealed))
  }

  /** `NameHashing.nameHashes`. */
  def nameHashes(c: HClass, optimizedSealed: Boolean): Array[NameHash] = {
    val defs = publicDefinitions(c)
    val location = Location(c.name, if (c.definitionType == DefinitionType.Trait || c.definitionType == DefinitionType.ClassDef) TypeName else TermName).hashCode
    val (implicits, regular) = defs.partition(_.modifiers.isImplicit)
    val forPatMat =
      if (optimizedSealed) group(defs.filter { case cl: HClass => cl.modifiers.isSealed; case _ => false }, location, UseScope.PatMatTarget, includeSealedChildren = true)
      else Nil
    val sealedChildren = !optimizedSealed
    (group(regular, location, UseScope.Default, sealedChildren) ++ group(implicits, location, UseScope.Implicit, sealedChildren) ++ forPatMat).toArray
  }

  private def group(defs: collection.Seq[HDefinition], location: Int, scope: UseScope, includeSealedChildren: Boolean): List[NameHash] = {
    val groups = new java.util.LinkedHashMap[String, collection.mutable.ArrayBuffer[HDefinition]]()
    defs.foreach { d =>
      val n = d.name
      val key = n.substring(n.lastIndexOf('.') + 1)
      var g = groups.get(key)
      if (g eq null) { g = new collection.mutable.ArrayBuffer[HDefinition]; groups.put(key, g) }
      g += d
    }
    val result = List.newBuilder[NameHash]
    groups.forEach { (name, ds) =>
      val f = new Fold(includeDefinitions = false, includeSealedChildren = includeSealedChildren, includeTraitBreakers = false)
      f.symmetric(ds.size) { i =>
        f.definition(ds(i))
        f.extend(location)
      }
      result += NameHash.of(name, scope, f.result)
    }
    result.result()
  }

  /** `NameHashing`'s `ExtractPublicDefinitions` visit, in the same order. */
  private def publicDefinitions(c: HClass): collection.Seq[HDefinition] = {
    val out = new collection.mutable.ArrayBuffer[HDefinition]
    val seen = java.util.Collections.newSetFromMap(new java.util.IdentityHashMap[HStructure, java.lang.Boolean])
    def tape(t: Tape): Unit = { var i = 0; while (i < t.structCount) { structure(t.structs(i)); i += 1 } }
    def structure(s: HStructure): Unit = if (seen.add(s)) {
      s.parents.get.foreach(tape)
      s.declared.get.foreach(definition)
      s.inherited.get.foreach(definition)
    }
    def definition(d: HDef): Unit = if (d.isNonPrivate) { out += d; tape(d.tape) }
    out += c
    tape(c.header)
    tape(c.typeParameters)
    tape(c.selfType.get)
    c.structure.get.structs.foreach(structure)
    out
  }

  private case class Location(className: String, nameType: NameType)
  private sealed trait NameType
  private case object TermName extends NameType
  private case object TypeName extends NameType

  private object Fold {
    def hashAPI(c: HClass, includeTraitBreakers: Boolean): Int = {
      val f = new Fold(includeDefinitions = true, includeSealedChildren = true, includeTraitBreakers = includeTraitBreakers)
      f.hash = 1
      f.hashClass(c)
      f.result
    }
  }

  /** `HashAPI`, over tapes. */
  private final class Fold(includeDefinitions: Boolean, includeSealedChildren: Boolean, includeTraitBreakers: Boolean) {
    var hash = 0
    private[this] val visitedStructures = new java.util.IdentityHashMap[HStructure, List[Int]]
    private[this] val visitedClasses = new java.util.IdentityHashMap[HClass, List[Int]]

    def extend(a: Int): Unit = hash = MurmurHash3.mix(hash, a)
    def result: Int = MurmurHash3.finalizeHash(hash, 1)

    private def visit[K](map: java.util.IdentityHashMap[K, List[Int]], k: K)(f: => Unit): Unit = {
      val prev = map.put(k, hash :: (map.get(k) match { case null => Nil; case l => l }))
      prev match {
        case x :: _ => extend(x)
        case _ =>
          f
          var hs = map.get(k)
          while (hs ne Nil) { extend(hs.head); hs = hs.tail }
          map.put(k, hash :: Nil)
      }
    }

    def tape(t: Tape): Unit = {
      val ints = t.ints
      var i = 0
      var s = 0
      while (i < ints.length) {
        while (s < t.structCount && t.at(s) == i) { structure(t.structs(s), includeDefinitions = true, isTrait = false); s += 1 }
        extend(ints(i))
        i += 1
      }
      while (s < t.structCount) { structure(t.structs(s), includeDefinitions = true, isTrait = false); s += 1 }
    }

    /** `hashType(t, includeDefinitions)`, which only matters for a bare structure. */
    private def tpe(t: Tape, includeDefinitions: Boolean): Unit = t.bareStructure match {
      case null => tape(t)
      case s    => structure(s, includeDefinitions, isTrait = false)
    }

    def structure(s: HStructure, includeDefinitions: Boolean, isTrait: Boolean): Unit =
      visit(visitedStructures, s) {
        extend(StructureHash)
        val parents = s.parents.get
        extend(parents.length)
        parents.foreach(tpe(_, includeDefinitions))
        if (includeDefinitions) {
          definitions(s.declared.get, isTrait)
          definitions(s.inherited.get, isTrait)
        }
      }

    private def definitions(ds: Array[HDef], isTrait: Boolean): Unit = {
      val defs = new collection.mutable.ArrayBuffer[HDef](ds.length)
      ds.foreach(d => if (d.isNonPrivate) defs += d)
      if (isTrait && includeTraitBreakers) ds.foreach(d => if (d.kind == KindTraitBreaker && d.isPrivate) defs += d)
      symmetric(defs.length)(i => tape(defs(i).tape))
    }

    def definition(d: HDefinition): Unit = d match {
      case c: HClass => tape(c.header); hashClass(c)
      case d: HDef   => tape(d.tape)
    }

    def hashClass(c: HClass): Unit = visit(visitedClasses, c) {
      extend(ClassHash)
      tape(c.typeParameters)
      tape(c.selfType.get)
      if (includeSealedChildren) {
        val cs = c.childrenOfSealedClass
        symmetric(cs.length)(i => tpe(cs(i), includeDefinitions))
      }
      structure(c.structure.get.structs(0), includeDefinitions, c.definitionType == DefinitionType.Trait)
    }

    /** `hashSymmetric`: each element from a fresh hash, combined as `MurmurHash3.unorderedHash` does, with its default seed. */
    def symmetric(n: Int)(element: Int => Unit): Unit = {
      val current = hash
      var a, b, count = 0
      var c = 1
      var i = 0
      while (i < n) {
        hash = 1
        element(i)
        val h = result
        a += h; b ^= h; c *= h | 1; count += 1
        i += 1
      }
      var h = MurmurHash3.traversableSeed
      h = MurmurHash3.mix(h, a)
      h = MurmurHash3.mix(h, b)
      h = MurmurHash3.mixLast(h, c)
      hash = current
      extend(MurmurHash3.finalizeHash(h, count))
    }
  }
}
