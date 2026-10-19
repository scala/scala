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
 * becomes a `Tape` instead: the ints `HashAPI` would `extend` with. Structures stay as nodes
 * because `HashAPI` keeps per-hash state about them (it hashes a structure met a second time by
 * the hash at the end of its first visit), and because `NameHashing` collects the members of
 * every structure it can reach.
 *
 * Tapes live in the `Context`'s arena, which is reused for every unit of a run, so a node costs
 * one small handle and no arrays.
 */
final class ApiHasher(ctx: Context) extends ApiVisitor {
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

  private[this] val b = ctx

  private[this] val theEmptyType = { b.int(EmptyTypeHash); b.result() }
  private[this] val theThis = { b.int(ThisPathHash); b.result() }

  def emptyType: Tape = theEmptyType
  def parameterRef(id: String): Tape = { b.int(ParameterRefHash); b.str(id); b.result() }
  def id(name: String): Tape = { b.int(IdPathHash); b.str(name); b.result() }
  def thisComponent: Tape = theThis
  def singleton(path: Array[Tape]): Tape = { b.int(SingletonHash); b.tapes(path); b.result() }
  def projection(prefix: Tape, id: String): Tape = { b.int(ProjectionHash); b.str(id); b.tape(prefix); b.result() }
  def parameterized(base: Tape, args: Array[Tape]): Tape = { b.int(ParameterizedHash); b.tape(base); b.tapes(args); b.result() }
  def annotated(base: Tape, annotations: Array[Tape]): Tape = { b.int(AnnotatedHash); b.tape(base); b.tapes(annotations); b.result() }
  def constant(base: Tape, value: String): Tape = { b.int(ConstantHash); b.str(value); b.tape(base); b.result() }
  def existential(base: Tape, clause: Array[Tape]): Tape = { b.int(ExistentialHash); b.tapes(clause); b.tape(base); b.result() }
  def polymorphic(base: Tape, parameters: Array[Tape]): Tape = { b.int(PolymorphicHash); b.tapes(parameters); b.tape(base); b.result() }
  def structure(parents: Lazy[Array[Tape]], declared: Lazy[Array[HDef]], inherited: Lazy[Array[HDef]]): Tape = {
    b.structure(new HStructure(parents, declared, inherited))
    b.result()
  }

  def annotationArgument(name: String, value: String): Tape = { b.str(name); b.str(value); b.result() }
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

  /** The hashes of a class extracted by this hasher. Call before the context is reset. */
  def hashes(c: HClass, optimizedSealed: Boolean): ClassHashes = ctx.hashes(c, optimizedSealed)
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

  /**
   * Arena encoding. An int other than `Esc` is extended as it is. `Esc` starts an escape:
   * `Esc, Lit` is `Esc` itself; `Esc, i` with `i >= 0` is the structure `refs(i)`;
   * `Esc, Ref, off, len` is the tape at `off`.
   */
  final val Esc = Int.MinValue + 0x5a5a5
  final val Lit = -1
  final val Ref = -2

  /** Tapes with at most this many ints are copied into their parent rather than referred to. */
  final val InlineLimit = 8

  /** The ints `HashAPI` extends its hash with for one node, at `ctx.ints(off until off + len)`. */
  final class Tape(val off: Int, val len: Int, val ctx: Context) {
    /** A structure on its own, which `HashAPI` hashes without its members in some positions. */
    def bareStructure: HStructure = ctx.bareStructure(off, len)

    /**
     * Equal when the `xsbti.api` nodes would be: `ExtractAPI` merges a field's and its accessors'
     * annotations in a set, relying on that equality, which compares structures by identity.
     */
    override def equals(o: Any): Boolean = o match {
      case t: Tape => (t eq this) || ctx.sameTape(off, len, t.off, t.len)
      case _       => false
    }
    override def hashCode: Int = ctx.tapeHash(off, len)
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

  private case class Location(className: String, nameType: NameType)
  private sealed trait NameType
  private case object TermName extends NameType
  private case object TypeName extends NameType

  /** `MurmurHash3.unorderedHash` with its default seed, from its running sums. */
  private def unordered(a: Int, b: Int, c: Int, count: Int): Int = {
    var h = MurmurHash3.traversableSeed
    h = MurmurHash3.mix(h, a)
    h = MurmurHash3.mix(h, b)
    h = MurmurHash3.mixLast(h, c)
    MurmurHash3.finalizeHash(h, count)
  }

  /**
   * The buffers of one run: the arena of tapes and the structures they refer to, which last for
   * a unit, and the hashing state, which lasts for a class. Not thread safe.
   */
  final class Context {
    private var ints = new Array[Int](1 << 16)
    private[this] var n = 0
    private[this] var start = 0
    private var refs = new Array[HStructure](256)
    private[this] var nr = 0

    /** Forgets the tapes of the last unit. */
    def reset(): Unit = {
      n = 0
      start = 0
      java.util.Arrays.fill(refs.asInstanceOf[Array[AnyRef]], 0, nr, null)
      nr = 0
    }

    private[this] val strHashes = new java.util.HashMap[String, Integer]()
    private def strHash(s: String): Int = {
      val cached = strHashes.get(s)
      if (cached ne null) cached.intValue
      else {
        val h = MurmurHash3.stringHash(s)
        strHashes.put(s, h)
        h
      }
    }

    private def ensure(k: Int): Unit =
      if (n + k > ints.length) ints = java.util.Arrays.copyOf(ints, math.max(ints.length * 2, n + k))
    private def raw(i: Int): Unit = { ensure(1); ints(n) = i; n += 1 }

    def int(i: Int): Unit = if (i == Esc) { raw(Esc); raw(Lit) } else raw(i)
    def str(s: String): Unit = int(strHash(s))
    def bool(x: Boolean): Unit = int(if (x) TrueHash else FalseHash)
    def structure(s: HStructure): Unit = {
      if (nr == refs.length) refs = java.util.Arrays.copyOf(refs, nr * 2)
      refs(nr) = s
      raw(Esc); raw(nr)
      nr += 1
    }
    /** Copies a small tape, and refers to a larger one. */
    def tape(t: Tape): Unit =
      if (t.len <= InlineLimit) {
        ensure(t.len)
        System.arraycopy(ints, t.off, ints, n, t.len)
        n += t.len
      } else {
        ensure(4)
        ints(n) = Esc; ints(n + 1) = Ref; ints(n + 2) = t.off; ints(n + 3) = t.len
        n += 4
      }
    def tapes(ts: Array[Tape]): Unit = {
      int(ts.length)
      var i = 0
      while (i < ts.length) { tape(ts(i)); i += 1 }
    }
    /** The tape written since the last one. */
    def result(): Tape = {
      val t = new Tape(start, n - start, this)
      start = n
      t
    }

    private[ApiHasher] def bareStructure(off: Int, len: Int): HStructure =
      if (len == 2 && ints(off) == Esc && ints(off + 1) >= 0) refs(ints(off + 1))
      else if (len == 4 && ints(off) == Esc && ints(off + 1) == Ref) bareStructure(ints(off + 2), ints(off + 3))
      else null

    /** Whether two tapes extend a hash alike. Encodings are canonical, so they are walked in step. */
    private[ApiHasher] def sameTape(off1: Int, len1: Int, off2: Int, len2: Int): Boolean = {
      if (len1 != len2) return false
      var i = 0
      while (i < len1) {
        val x = ints(off1 + i)
        if (x != ints(off2 + i)) return false
        if (x != Esc) i += 1
        else {
          val k1 = ints(off1 + i + 1)
          val k2 = ints(off2 + i + 1)
          if (k1 >= 0 && k2 >= 0) {
            if (refs(k1) ne refs(k2)) return false
            i += 2
          } else if (k1 != k2) return false
          else if (k1 == Ref) {
            if (!sameTape(ints(off1 + i + 2), ints(off1 + i + 3), ints(off2 + i + 2), ints(off2 + i + 3))) return false
            i += 4
          } else i += 2
        }
      }
      true
    }

    private[ApiHasher] def tapeHash(off: Int, len: Int): Int = {
      var h = 0
      var i = off
      val end = off + len
      while (i < end) {
        val x = ints(i)
        if (x != Esc) { h = h * 31 + x; i += 1 }
        else {
          val k = ints(i + 1)
          if (k == Ref) { h = h * 31 + tapeHash(ints(i + 2), ints(i + 3)); i += 4 }
          else if (k >= 0) { h = h * 31 + System.identityHashCode(refs(k)); i += 2 }
          else { h = h * 31 + Esc; i += 2 }
        }
      }
      h
    }

    // `HashAPI`, over the arena.

    private[this] var hash = 0
    private[this] var includeDefinitions = true
    private[this] var includeSealedChildren = true
    private[this] var includeTraitBreakers = false
    private[this] val visitedStructures = new java.util.IdentityHashMap[HStructure, List[Int]]
    private[this] val visitedClasses = new java.util.IdentityHashMap[HClass, List[Int]]

    private def startHash(includeDefinitions: Boolean, includeSealedChildren: Boolean, includeTraitBreakers: Boolean): Unit = {
      this.includeDefinitions = includeDefinitions
      this.includeSealedChildren = includeSealedChildren
      this.includeTraitBreakers = includeTraitBreakers
      if (!visitedStructures.isEmpty) visitedStructures.clear()
      if (!visitedClasses.isEmpty) visitedClasses.clear()
    }

    def hashes(c: HClass, optimizedSealed: Boolean): ClassHashes = {
      val apiHash = hashAPI(c, includeTraitBreakers = false)
      val extraHash =
        if (c.definitionType == DefinitionType.Trait) hashAPI(c, includeTraitBreakers = true) else apiHash
      new ClassHashes(c.name, c.definitionType, apiHash, extraHash, nameHashes(c, optimizedSealed))
    }

    private def hashAPI(c: HClass, includeTraitBreakers: Boolean): Int = {
      startHash(includeDefinitions = true, includeSealedChildren = true, includeTraitBreakers = includeTraitBreakers)
      hash = 1
      hashClass(c)
      finished
    }

    private def extend(a: Int): Unit = hash = MurmurHash3.mix(hash, a)
    private def finished: Int = MurmurHash3.finalizeHash(hash, 1)

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

    private def hashTape(t: Tape): Unit = walk(t.off, t.len)
    private def walk(off: Int, len: Int): Unit = {
      var i = off
      val end = off + len
      while (i < end) {
        val x = ints(i)
        if (x != Esc) { extend(x); i += 1 }
        else {
          val k = ints(i + 1)
          if (k >= 0) { structure(refs(k), includeDefinitions = true, isTrait = false); i += 2 }
          else if (k == Ref) { walk(ints(i + 2), ints(i + 3)); i += 4 }
          else { extend(Esc); i += 2 }
        }
      }
    }

    /** `hashType(t, includeDefinitions)`, which only matters for a bare structure. */
    private def tpe(t: Tape, includeDefinitions: Boolean): Unit = t.bareStructure match {
      case null => hashTape(t)
      case s    => structure(s, includeDefinitions, isTrait = false)
    }

    private def structure(s: HStructure, includeDefinitions: Boolean, isTrait: Boolean): Unit =
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

    /** `hashDefinitions`: the non-private definitions, then a trait's private trait breakers. */
    private def definitions(ds: Array[HDef], isTrait: Boolean): Unit = {
      val current = hash
      var a, b, count = 0
      var c = 1
      var i = 0
      while (i < ds.length) {
        val d = ds(i)
        if (d.isNonPrivate) {
          hash = 1; hashTape(d.tape)
          val h = finished
          a += h; b ^= h; c *= h | 1; count += 1
        }
        i += 1
      }
      if (isTrait && includeTraitBreakers) {
        i = 0
        while (i < ds.length) {
          val d = ds(i)
          if (d.kind == KindTraitBreaker && d.isPrivate) {
            hash = 1; hashTape(d.tape)
            val h = finished
            a += h; b ^= h; c *= h | 1; count += 1
          }
          i += 1
        }
      }
      hash = current
      extend(unordered(a, b, c, count))
    }

    private def definition(d: HDefinition): Unit = d match {
      case c: HClass => hashTape(c.header); hashClass(c)
      case d: HDef   => hashTape(d.tape)
    }

    private def hashClass(c: HClass): Unit =
      if (enter(visitedClasses, c)) {
        extend(ClassHash)
        hashTape(c.typeParameters)
        hashTape(c.selfType.get)
        if (includeSealedChildren) {
          val cs = c.childrenOfSealedClass
          val current = hash
          var a, b, count = 0
          var k = 1
          var i = 0
          while (i < cs.length) {
            hash = 1; tpe(cs(i), includeDefinitions)
            val h = finished
            a += h; b ^= h; k *= h | 1; count += 1
            i += 1
          }
          hash = current
          extend(unordered(a, b, k, count))
        }
        structure(c.structure.get.bareStructure, includeDefinitions, c.definitionType == DefinitionType.Trait)
        exit(visitedClasses, c)
      }

    // `NameHashing`, over the arena.

    private[this] val defs = new collection.mutable.ArrayBuffer[HDefinition]
    private[this] val seen = new java.util.IdentityHashMap[HStructure, HStructure]
    private[this] var next = new Array[Int](64)
    private[this] val first = new java.util.HashMap[String, Integer]()
    private[this] val out = new collection.mutable.ArrayBuffer[NameHash]

    private def nameHashes(c: HClass, optimizedSealed: Boolean): Array[NameHash] = {
      publicDefinitions(c)
      val location = Location(c.name, if (c.definitionType == DefinitionType.Trait || c.definitionType == DefinitionType.ClassDef) TypeName else TermName).hashCode
      out.clear()
      val sealedChildren = !optimizedSealed
      group(SelectDefault, location, UseScope.Default, sealedChildren)
      group(SelectImplicit, location, UseScope.Implicit, sealedChildren)
      if (optimizedSealed) group(SelectSealedClass, location, UseScope.PatMatTarget, includeSealedChildren = true)
      out.toArray
    }

    private def selects(d: HDefinition, selection: Int): Boolean = selection match {
      case SelectDefault  => !d.modifiers.isImplicit
      case SelectImplicit => d.modifiers.isImplicit
      case _              => d.isInstanceOf[HClass] && d.modifiers.isSealed
    }

    /** One name hash per local name among the selected definitions, each group in visit order. */
    private def group(selection: Int, location: Int, scope: UseScope, includeSealedChildren: Boolean): Unit = {
      val n = defs.length
      if (next.length < n) next = new Array[Int](math.max(n, next.length * 2))
      first.clear()
      var i = n - 1
      while (i >= 0) {
        val d = defs(i)
        if (selects(d, selection)) {
          val key = d.name.substring(d.name.lastIndexOf('.') + 1)
          val prev = first.put(key, i)
          next(i) = if (prev eq null) -1 else prev.intValue
        }
        i -= 1
      }
      val it = first.entrySet.iterator
      while (it.hasNext) {
        val e = it.next()
        startHash(includeDefinitions = false, includeSealedChildren = includeSealedChildren, includeTraitBreakers = false)
        var a, b, count = 0
        var c = 1
        var j = e.getValue.intValue
        while (j >= 0) {
          hash = 1
          definition(defs(j))
          extend(location)
          val h = finished
          a += h; b ^= h; c *= h | 1; count += 1
          j = next(j)
        }
        hash = 0
        extend(unordered(a, b, c, count))
        out += NameHash.of(e.getKey, scope, finished)
      }
    }

    /** `NameHashing`'s `ExtractPublicDefinitions` visit, in the same order, into `defs`. */
    private def publicDefinitions(c: HClass): Unit = {
      defs.clear()
      seen.clear()
      defs += c
      structuresIn(c.header)
      structuresIn(c.typeParameters)
      structuresIn(c.selfType.get)
      publicIn(c.structure.get.bareStructure)
    }
    private def structuresIn(t: Tape): Unit = structuresIn(t.off, t.len)
    private def structuresIn(off: Int, len: Int): Unit = {
      var i = off
      val end = off + len
      while (i < end) {
        if (ints(i) != Esc) i += 1
        else {
          val k = ints(i + 1)
          if (k >= 0) { publicIn(refs(k)); i += 2 }
          else if (k == Ref) { structuresIn(ints(i + 2), ints(i + 3)); i += 4 }
          else i += 2
        }
      }
    }
    private def publicIn(s: HStructure): Unit = if (seen.put(s, s) eq null) {
      val parents = s.parents.get
      var i = 0
      while (i < parents.length) { structuresIn(parents(i)); i += 1 }
      publicIn(s.declared.get)
      publicIn(s.inherited.get)
    }
    private def publicIn(ds: Array[HDef]): Unit = {
      var i = 0
      while (i < ds.length) {
        val d = ds(i)
        if (d.isNonPrivate) { defs += d; structuresIn(d.tape) }
        i += 1
      }
    }
  }

  private final val SelectDefault = 0
  private final val SelectImplicit = 1
  private final val SelectSealedClass = 2
}
