/* NSC -- new Scala compiler
 * Copyright 2005-2014 LAMP/EPFL
 * @author  Jason Zaugg
 */
package scala.reflect.internal
package tpe

import Flags._
import util.Statistics
import TypesStats._

trait FindMembers {
  this: SymbolTable =>

  /** Implementatation of `Type#{findMember, findMembers}` */
  private[internal] abstract class FindMemberBase[T](tpe: Type, name: Name, excludedFlags: Long, requiredFlags: Long) {
    protected val initBaseClasses: List[Symbol] = tpe.baseClasses

    // The first base class, or the symbol of the ThisType
    private[this] var _selectorClass: Symbol = null
    private def selectorClass: Symbol = {
      if (_selectorClass eq null) {
        _selectorClass = tpe match {
          case tt: ThisType => tt.sym // SI-7507 the first base class is not necessarily the selector class.
          case _            => initBaseClasses.head
        }
      }
      _selectorClass
    }

    // Cache for the narrowed type of `tp` (in `tp.findMember`).
    // This is needed to avoid mismatched existential types are reported in SI-5330.
    private[this] var _self: Type = null
    protected def self: Type = {
      // TODO: use narrow only for modules? (correct? efficiency gain?) (<-- Note: this comment predates SI-5330)
      if (_self eq null) _self = narrowForFindMember(tpe)
      _self
    }

    // Main entry point
    def apply(): T = {
      if (Statistics.canEnable) Statistics.incCounter(findMemberCount)
      val start = if (Statistics.canEnable) Statistics.pushTimer(typeOpsStack, findMemberNanos) else null
      try searchConcreteThenDeferred
      finally if (Statistics.canEnable) Statistics.popTimer(typeOpsStack, start)
    }

    protected def result: T

    // SLS 5.1.3 First, a concrete definition always overrides an abstract definition
    private def searchConcreteThenDeferred: T = {
      val deferredSeen = walkBaseClasses(requiredFlags, excludedFlags | DEFERRED)
      if (deferredSeen) // OPT: the `if` avoids a second pass if the first pass didn't spot any candidates.
        walkBaseClasses(requiredFlags | DEFERRED, excludedFlags & ~(DEFERRED.toLong))
      result
    }

    /*
     * Walk up through the decls of each base class.
     *
     * Called in two passes: first excluding deferred, then mandating it.
     *
     * @return true, if a potential deferred member was seen on the first pass that calls for a second pass.
     */
    private def walkBaseClasses(required: Long, excluded: Long): Boolean = {
      var bcs = initBaseClasses

      // Has we seen a candidate deferred member?
      var deferredSeen = false

      while (!bcs.isEmpty) {
        val currentBaseClass = bcs.head
        val decls = currentBaseClass.info.decls
        val findAll = name == nme.ANYname
        var entry = if (findAll) decls.elems else decls.lookupEntry(name)
        while (entry ne null) {
          val sym = entry.sym
          val flags = sym.flags
          val meetsRequirements = (flags & required) == required
          if (meetsRequirements) {
            val excl: Long = flags & excluded
            val isExcluded: Boolean = excl != 0L
            if (!isExcluded && isPotentialMember(sym, flags, currentBaseClass)) {
              if (shortCircuit(sym)) return false
              else addMemberIfNew(sym)
            } else if (excl == DEFERRED) {
              deferredSeen = true
            }
          }
          entry = if (findAll) entry.next else decls lookupNextEntry entry
        }

        bcs = bcs.tail
      }
      deferredSeen
    }

    /* Should this symbol be returned immediately as the sole result? */
    protected def shortCircuit(sym: Symbol): Boolean

    /* Add this member to the final result, unless an already-found member matches it. */
    protected def addMemberIfNew(sym: Symbol): Unit

    // Is `sym` a potentially member of `baseClass`?
    //
    // Q. When does a potential member fail to be a an actual member?
    // A. if it is subsumed by an member in a subclass.
    private def isPotentialMember(sym: Symbol, flags: Long, owner: Symbol): Boolean = {
      // conservatively (performance wise) doing this with flags masks rather than `sym.isPrivate`
      // to avoid multiple calls to `Symbol#flags`.
      val isPrivateLocal = (flags & PrivateLocal) == PrivateLocal

      def admitPrivateLocal(sym: Symbol): Boolean =
        (
             (selectorClass == owner)
          || !isPrivateLocal
          || selectorClass.hasTransOwner(owner)     // private[this] only a member from within the class (incorrect!)
       )

      // TODO first condition incorrect wrt self types! In neg/t7507, bippy should not be a member!
      //      The condition, or a close variant thereof, will still be needed to prevent inheritance of constructors.
      owner == initBaseClasses.head || admitPrivateLocal(sym) && sym.name != nme.CONSTRUCTOR
    }

    // True unless the already-found member of type `memberType` matches the candidate symbol `other`.
    protected def isNewMember(member: Symbol, other: Symbol): Boolean =
      (    (other ne member)
        && (    (member.owner eq other.owner)
             || (other.flags & PRIVATE) != 0
             || !(memberTypeLow(member) matches memberTypeHi(other))
           )
      )

    // Cache for the member type of a candidate member when comparing against multiple, already-found existing members
    //
    // TODO this cache is probably unnecessary, `tp.memberType(sym: MethodSymbol)` is already cached internally.
    private[this] var _memberTypeHiCache: Type = null
    private[this] var _memberTypeHiCacheSym: Symbol = null

    protected def memberTypeHi(sym: Symbol): Type = {
      if (_memberTypeHiCacheSym ne sym) {
        _memberTypeHiCache = self.memberType(sym)
        _memberTypeHiCacheSym = sym
      }
      _memberTypeHiCache
    }

    // member type of the LHS of `matches` call. This is an extension point to enable a cache in
    // FindMember.
    protected def memberTypeLow(sym: Symbol): Type = self.memberType(sym)
  }

  private[reflect] final class FindMembers(tpe: Type, excludedFlags: Long, requiredFlags: Long)
    extends FindMemberBase[Scope](tpe, nme.ANYname, excludedFlags, requiredFlags) {
    private[this] var _membersScope: Scope   = null
    private def membersScope: Scope = {
      if (_membersScope eq null) _membersScope = newFindMemberScope
      _membersScope
    }

    protected def shortCircuit(sym: Symbol): Boolean = false
    protected def result: Scope = membersScope

    protected def addMemberIfNew(sym: Symbol): Unit = {
      val members = membersScope
      var others = members.lookupEntry(sym.name)
      var isNew = true
      while (others ne null) {
        val member = others.sym
        if (!isNewMember(member, sym))
          isNew = false
        others = members lookupNextEntry others // next existing member with the same name.
      }
      if (isNew) members.enter(sym)
    }
  }

  private[reflect] final class FindMember(tpe: Type, name: Name, excludedFlags: Long, requiredFlags: Long, stableOnly: Boolean)
    extends FindMemberBase[Symbol](tpe, name, excludedFlags, requiredFlags) {
    // Gathering the results into a hand rolled ListBuffer
    // TODO Try just using a ListBuffer to see if this low-level-ness is worth it.
    private[this] var member0: Symbol       = NoSymbol
    private[this] var members: List[Symbol] = null
    private[this] var lastM: ::[Symbol]     = null

    private def clearAndAddResult(sym: Symbol): Unit = {
      member0 = sym
      members = null
      lastM = null
    }

    protected def shortCircuit(sym: Symbol): Boolean = (name.isTypeName || (stableOnly && sym.isStable && !sym.hasVolatileType)) && {
      clearAndAddResult(sym)
      true
    }

    protected def addMemberIfNew(sym: Symbol): Unit =
      if (member0 eq NoSymbol) {
        member0 = sym // The first found member
      } else if (members eq null) {
        // We've found exactly one member so far...
        if (isNewMember(member0, sym)) {
          // ... make that two.
          lastM = new ::(sym, null)
          members = member0 :: lastM
        }
      } else {
        // Already found 2 or more members
        var ms: List[Symbol] = members

        var isNew = true
        while ((ms ne null) && isNew) {
          val member = ms.head
          if (!isNewMember(member, sym))
            isNew = false
          ms = ms.tail
        }
        if (isNew) {
          val lastM1 = new ::(sym, null)
          lastM.tl = lastM1
          lastM = lastM1
        }
      }

    // Cache for the member type of the first member we find.
    private[this] var _member0Tpe: Type = null
    private[this] def member0Tpe: Type = {
      assert(member0 != null)
      if (_member0Tpe eq null) _member0Tpe = self.memberType(member0)
      _member0Tpe
    }
    
    override protected def memberTypeLow(sym: Symbol): Type =
      if (sym eq member0) member0Tpe else super.memberTypeLow(sym)

    // Assemble the result from the hand-rolled ListBuffer
    protected def result: Symbol = if (members eq null) {
      if (member0 == NoSymbol) {
        if (Statistics.canEnable) Statistics.incCounter(noMemberCount)
        NoSymbol
      } else member0
    } else {
      if (Statistics.canEnable) Statistics.incCounter(multMemberCount)
      lastM.tl = Nil
      initBaseClasses.head.newOverloaded(tpe, members)
    }
  }
}
