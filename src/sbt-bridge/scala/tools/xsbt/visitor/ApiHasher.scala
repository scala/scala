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
 * becomes a `Tape` instead: the ints `HashAPI` would `extend` with, with the structures and the
 * larger child nodes among them kept by reference. Structures stay as nodes because `HashAPI` keeps per-hash state about them (it
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
    private[this] var kids: Array[AnyRef] = new Array[AnyRef](8)
    private[this] var at: Array[Int] = new Array[Int](8)
    private[this] var nk = 0

    def int(i: Int): Unit = {
      if (n == ints.length) ints = java.util.Arrays.copyOf(ints, n * 2)
      ints(n) = i
      n += 1
    }
    def str(s: String): Unit = int(ApiHasher.this.str(s))
    def bool(x: Boolean): Unit = int(if (x) TrueHash else FalseHash)
    private def kid(k: AnyRef, pos: Int): Unit = {
      if (nk == kids.length) { kids = java.util.Arrays.copyOf(kids, nk * 2); at = java.util.Arrays.copyOf(at, nk * 2) }
      kids(nk) = k
      at(nk) = pos
      nk += 1
    }
    /** Copies a small child, and refers to a larger one. */
    def tape(t: Tape): Unit =
      if (t.ints.length + t.kids.length > InlineLimit) kid(t, n)
      else {
        val base = n
        var i = 0
        while (i < t.kids.length) { kid(t.kids(i), base + t.at(i)); i += 1 }
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
        if (nk == 0) new Tape(java.util.Arrays.copyOf(ints, n), Tape.NoKids, Tape.NoAt)
        else new Tape(java.util.Arrays.copyOf(ints, n), java.util.Arrays.copyOf(kids, nk), java.util.Arrays.copyOf(at, nk))
      java.util.Arrays.fill(kids, 0, nk, null)
      n = 0
      nk = 0
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

  /** Children with at most this many ints and children of their own are copied into their parent. */
  final val InlineLimit = 8

  /**
   * The ints `HashAPI` extends its hash with for one node. `kids(i)`, a structure or a larger
   * child node, comes before `ints(at(i))`.
   */
  final class Tape(val ints: Array[Int], val kids: Array[AnyRef], val at: Array[Int]) {
    /** A structure on its own, which `HashAPI` hashes without its members in some positions. */
    def bareStructure: HStructure =
      if (ints.length == 0 && kids.length == 1) kids(0) match {
        case s: HStructure => s
        case t: Tape       => t.bareStructure
        case x             => throw new MatchError(x)
      }
      else null

    /**
     * Equal when the `xsbti.api` nodes would be: `ExtractAPI` merges a field's and its accessors'
     * annotations in a set, relying on that equality, which compares structures by identity.
     */
    override def equals(o: Any): Boolean = o match {
      case t: Tape =>
        java.util.Arrays.equals(ints, t.ints) && java.util.Arrays.equals(at, t.at) && java.util.Arrays.equals(kids, t.kids)
      case _ => false
    }
    override def hashCode: Int = java.util.Arrays.hashCode(ints)
  }
  object Tape {
    val NoKids = new Array[AnyRef](0)
    val NoAt = new Array[Int](0)
    def of(i0: Int): Tape = new Tape(Array(i0), NoKids, NoAt)
    def of(i0: Int, i1: Int): Tape = new Tape(Array(i0, i1), NoKids, NoAt)
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
    val f = new Fold
    val apiHash = f.hashAPI(c, includeTraitBreakers = false)
    val extraHash =
      if (c.definitionType == DefinitionType.Trait) f.hashAPI(c, includeTraitBreakers = true) else apiHash
    new ClassHashes(c.name, c.definitionType, apiHash, extraHash, nameHashes(f, c, optimizedSealed))
  }

  /** `NameHashing.nameHashes`. */
  private def nameHashes(f: Fold, c: HClass, optimizedSealed: Boolean): Array[NameHash] = {
    val defs = publicDefinitions(c)
    val location = Location(c.name, if (c.definitionType == DefinitionType.Trait || c.definitionType == DefinitionType.ClassDef) TypeName else TermName).hashCode
    val result = new collection.mutable.ArrayBuffer[NameHash](defs.length)
    val sealedChildren = !optimizedSealed
    group(f, defs, d => !d.modifiers.isImplicit, location, UseScope.Default, sealedChildren, result)
    group(f, defs, d => d.modifiers.isImplicit, location, UseScope.Implicit, sealedChildren, result)
    if (optimizedSealed)
      group(f, defs, { case cl: HClass => cl.modifiers.isSealed; case _ => false }, location, UseScope.PatMatTarget, includeSealedChildren = true, result)
    result.toArray
  }

  /** One name hash per local name among the selected definitions, each grouped in visit order. */
  private def group(
      f: Fold,
      defs: Array[HDefinition],
      select: HDefinition => Boolean,
      location: Int,
      scope: UseScope,
      includeSealedChildren: Boolean,
      result: collection.mutable.ArrayBuffer[NameHash]
  ): Unit = {
    val n = defs.length
    val next = new Array[Int](n)
    val first = new java.util.HashMap[String, Integer]()
    var i = n - 1
    while (i >= 0) {
      val d = defs(i)
      if (select(d)) {
        val key = d.name.substring(d.name.lastIndexOf('.') + 1)
        val prev = first.put(key, i)
        next(i) = if (prev eq null) -1 else prev.intValue
      }
      i -= 1
    }
    first.forEach { (key, head) =>
      f.reset(includeDefinitions = false, includeSealedChildren = includeSealedChildren, includeTraitBreakers = false)
      f.hash = 0
      val current = f.hash
      var a, b, count = 0
      var c = 1
      var j = head.intValue
      while (j >= 0) {
        f.hash = 1
        f.definition(defs(j))
        f.extend(location)
        val h = f.result
        a += h; b ^= h; c *= h | 1; count += 1
        j = next(j)
      }
      f.hash = current
      f.extend(unordered(a, b, c, count))
      result += NameHash.of(key, scope, f.result)
    }
  }

  /** `MurmurHash3.unorderedHash` with its default seed, from its running sums. */
  private def unordered(a: Int, b: Int, c: Int, count: Int): Int = {
    var h = MurmurHash3.traversableSeed
    h = MurmurHash3.mix(h, a)
    h = MurmurHash3.mix(h, b)
    h = MurmurHash3.mixLast(h, c)
    MurmurHash3.finalizeHash(h, count)
  }

  /** `NameHashing`'s `ExtractPublicDefinitions` visit, in the same order. */
  private def publicDefinitions(c: HClass): Array[HDefinition] = {
    val out = new collection.mutable.ArrayBuffer[HDefinition]
    val seen = new java.util.IdentityHashMap[HStructure, HStructure]
    def tape(t: Tape): Unit = {
      var i = 0
      while (i < t.kids.length) {
        t.kids(i) match {
          case s: HStructure => structure(s)
          case k: Tape       => tape(k)
          case x             => throw new MatchError(x)
        }
        i += 1
      }
    }
    def structure(s: HStructure): Unit = if (seen.put(s, s) eq null) {
      s.parents.get.foreach(tape)
      s.declared.get.foreach(definition)
      s.inherited.get.foreach(definition)
    }
    def definition(d: HDef): Unit = if (d.isNonPrivate) { out += d; tape(d.tape) }
    out += c
    tape(c.header)
    tape(c.typeParameters)
    tape(c.selfType.get)
    structure(c.structure.get.bareStructure)
    out.toArray
  }

  private case class Location(className: String, nameType: NameType)
  private sealed trait NameType
  private case object TermName extends NameType
  private case object TypeName extends NameType

  /** `HashAPI`, over tapes. Reused across the hashes of one class. */
  private final class Fold {
    var hash = 0
    private[this] var includeDefinitions = true
    private[this] var includeSealedChildren = true
    private[this] var includeTraitBreakers = false
    private[this] var visitedStructures: java.util.IdentityHashMap[HStructure, List[Int]] = null
    private[this] var visitedClasses: java.util.IdentityHashMap[HClass, List[Int]] = null

    def reset(includeDefinitions: Boolean, includeSealedChildren: Boolean, includeTraitBreakers: Boolean): Unit = {
      this.includeDefinitions = includeDefinitions
      this.includeSealedChildren = includeSealedChildren
      this.includeTraitBreakers = includeTraitBreakers
      if ((visitedStructures ne null) && !visitedStructures.isEmpty) visitedStructures.clear()
      if ((visitedClasses ne null) && !visitedClasses.isEmpty) visitedClasses.clear()
    }

    def hashAPI(c: HClass, includeTraitBreakers: Boolean): Int = {
      reset(includeDefinitions = true, includeSealedChildren = true, includeTraitBreakers = includeTraitBreakers)
      hash = 1
      hashClass(c)
      result
    }

    def extend(a: Int): Unit = hash = MurmurHash3.mix(hash, a)
    def result: Int = MurmurHash3.finalizeHash(hash, 1)

    /** `HashAPI.visit`, up to `hashF`: false, after extending, if `k` was visited before. */
    private def enter[K](map: java.util.IdentityHashMap[K, List[Int]], k: K): Boolean = {
      val prev = map.put(k, hash :: (map.get(k) match { case null => Nil; case l => l }))
      prev match {
        case x :: _ => extend(x); false
        case _      => true
      }
    }
    /** `HashAPI.visit`, after `hashF`. */
    private def exit[K](map: java.util.IdentityHashMap[K, List[Int]], k: K): Unit = {
      var hs = map.get(k)
      while (hs ne Nil) { extend(hs.head); hs = hs.tail }
      map.put(k, hash :: Nil)
    }

    def tape(t: Tape): Unit = {
      val ints = t.ints
      var i = 0
      var s = 0
      while (i < ints.length) {
        while (s < t.kids.length && t.at(s) == i) { kid(t.kids(s)); s += 1 }
        extend(ints(i))
        i += 1
      }
      while (s < t.kids.length) { kid(t.kids(s)); s += 1 }
    }
    private def kid(k: AnyRef): Unit = k match {
      case s: HStructure => structure(s, includeDefinitions = true, isTrait = false)
      case t: Tape       => tape(t)
      case x             => throw new MatchError(x)
    }

    /** `hashType(t, includeDefinitions)`, which only matters for a bare structure. */
    private def tpe(t: Tape, includeDefinitions: Boolean): Unit = t.bareStructure match {
      case null => tape(t)
      case s    => structure(s, includeDefinitions, isTrait = false)
    }

    def structure(s: HStructure, includeDefinitions: Boolean, isTrait: Boolean): Unit = {
      if (visitedStructures eq null) visitedStructures = new java.util.IdentityHashMap[HStructure, List[Int]]
      if (enter(visitedStructures, s)) {
        extend(StructureHash)
        val parents = s.parents.get
        extend(parents.length)
        var i = 0
        while (i < parents.length) { tpe(parents(i), includeDefinitions); i += 1 }
        if (includeDefinitions) {
          definitions(s.declared.get, isTrait)
          definitions(s.inherited.get, isTrait)
        }
        exit(visitedStructures, s)
      }
    }

    /** `hashDefinitions`: the non-private definitions, then a trait's private trait breakers. */
    private def definitions(ds: Array[HDef], isTrait: Boolean): Unit = {
      val current = hash
      var a, b, count = 0
      var c = 1
      var i = 0
      while (i < ds.length) {
        val d = ds(i)
        if (d.isNonPrivate) {
          hash = 1; tape(d.tape)
          val h = result
          a += h; b ^= h; c *= h | 1; count += 1
        }
        i += 1
      }
      if (isTrait && includeTraitBreakers) {
        i = 0
        while (i < ds.length) {
          val d = ds(i)
          if (d.kind == KindTraitBreaker && d.isPrivate) {
            hash = 1; tape(d.tape)
            val h = result
            a += h; b ^= h; c *= h | 1; count += 1
          }
          i += 1
        }
      }
      hash = current
      extend(unordered(a, b, c, count))
    }

    def definition(d: HDefinition): Unit = d match {
      case c: HClass => tape(c.header); hashClass(c)
      case d: HDef   => tape(d.tape)
    }

    def hashClass(c: HClass): Unit = {
      if (visitedClasses eq null) visitedClasses = new java.util.IdentityHashMap[HClass, List[Int]]
      if (enter(visitedClasses, c)) {
        extend(ClassHash)
        tape(c.typeParameters)
        tape(c.selfType.get)
        if (includeSealedChildren) {
          val cs = c.childrenOfSealedClass
          val current = hash
          var a, b, count = 0
          var k = 1
          var i = 0
          while (i < cs.length) {
            hash = 1; tpe(cs(i), includeDefinitions)
            val h = result
            a += h; b ^= h; k *= h | 1; count += 1
            i += 1
          }
          hash = current
          extend(unordered(a, b, k, count))
        }
        structure(c.structure.get.bareStructure, includeDefinitions, c.definitionType == DefinitionType.Trait)
        exit(visitedClasses, c)
      }
    }
  }
}
