/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Iulian Dragos
 */


package scala.tools.nsc
package backend.opt

import scala.util.control.Breaks._
import scala.collection.{ mutable, immutable }
import mutable.{ HashMap, HashSet }
import scala.tools.nsc.symtab._

/**
 *  @author Iulian Dragos
 */
abstract class Inliners extends SubComponent {
  import global._
  import icodes._
  import icodes.opcodes._
  import definitions.{
    NullClass, NothingClass, ObjectClass,
    PredefModule, RuntimePackage, ScalaInlineClass, ScalaNoInlineClass,
    isFunctionType
  }

  val phaseName = "inliner"

  /** Debug - for timing the inliner. */
  private def timed[T](s: String, body: => T): T = {
    val t1 = System.currentTimeMillis()
    val res = body
    val t2 = System.currentTimeMillis()
    val ms = (t2 - t1).toInt
    if (ms >= MAX_INLINE_MILLIS)
      println("%s: %d milliseconds".format(s, ms))

    res
  }

  /* A warning threshold */
  private final val MAX_INLINE_MILLIS = 2000

  /** The maximum size in basic blocks of methods considered for inlining. */
  final val MAX_INLINE_SIZE = 16

  /** Maximum loop iterations. */
  final val MAX_INLINE_RETRY = 15

  /** Small method size (in blocks) */
  val SMALL_METHOD_SIZE = 1

  /** Create a new phase */
  override def newPhase(p: Phase) = new InliningPhase(p)

  /** The Inlining phase.
   */
  class InliningPhase(prev: Phase) extends ICodePhase(prev) {
    def name = phaseName
    val inliner = new Inliner

    override def apply(c: IClass) {
      inliner analyzeClass c
    }
  }

  def isBottomType(sym: Symbol) = sym == NullClass || sym == NothingClass
  def posToStr(pos: util.Position) = if (pos.isDefined) pos.point.toString else "<nopos>"

  /** Is the given class a closure? */
  def isClosureClass(cls: Symbol): Boolean =
    cls.isFinal && cls.isSynthetic && !cls.isModuleClass && cls.isAnonymousFunction

  /**
   * Simple inliner.
   */
  class Inliner {
    object NonPublicRefs extends Enumeration {
      val Public, Protected, Private = Value

      /** Cache whether a method calls private members. */
      val usesNonPublics: mutable.Map[IMethod, Value] = new HashMap
    }
    import NonPublicRefs._

    /* fresh name counter */
    val fresh = new HashMap[String, Int]
    var count = 0
    def freshName(s: String) = {
      val count = fresh.getOrElseUpdate(s, 0)
      fresh(s) += 1
      s + count
    }

    private def hasInline(sym: Symbol)    = sym hasAnnotation ScalaInlineClass
    private def hasNoInline(sym: Symbol)  = sym hasAnnotation ScalaNoInlineClass

    /** The current iclass */
    private var currentIClazz: IClass = _
    private def warn(pos: Position, msg: String) = currentIClazz.cunit.warning(pos, msg)

    def analyzeClass(cls: IClass): Unit =
      if (settings.inline.value) {
        if (settings.debug.value)
        	log("Analyzing " + cls)

        this.currentIClazz = cls
        cls.methods filterNot (_.symbol.isConstructor) foreach analyzeMethod
      }

    val tfa   = new analysis.MethodTFA()
    tfa.stat  = settings.Ystatistics.value

    // how many times have we already inlined this method here?
    private val inlinedMethodCount: mutable.Map[Symbol, Int] = new HashMap[Symbol, Int] {
    	override def default(k: Symbol) = 0
    }

    def analyzeMethod(m: IMethod): Unit = {
      var retry = false
      var count = 0
      fresh.clear
      inlinedMethodCount.clear
      val caller = new IMethodInfo(m)
      var info: tfa.lattice.Elem = null

      def analyzeInc(msym: Symbol, i: Instruction, bb: BasicBlock) = {
        def paramTypes  = msym.info.paramTypes
        val receiver    = (info.stack.types drop paramTypes.length).head match {
          case REFERENCE(s) => s
          case _            => NoSymbol
        }
        val concreteMethod  = lookupImplFor(msym, receiver)

        def warnNoInline(reason: String) = {
          if (hasInline(msym) && !caller.isBridge)
            warn(i.pos, "Could not inline required method %s because %s.".format(msym.originalName.decode, reason))
        }

        if (shouldLoadImplFor(concreteMethod, receiver))
          icodes.icode(receiver, true)

        def isAvailable = icodes available receiver
        def isCandidate = isClosureClass(receiver) || concreteMethod.isEffectivelyFinal || receiver.isFinal
        def isApply     = concreteMethod.name == nme.apply
        def isCountable = !(isClosureClass(receiver) && isApply)   // only count non-closures

        if (settings.debug.value)
          log("Treating " + i
              + "\n\treceiver: " + receiver
              + "\n\ticodes.available: " + isAvailable
              + "\n\tconcreteMethod.isEffectivelyFinal: " + concreteMethod.isEffectivelyFinal)

        if (isAvailable && isCandidate) {
          lookupIMethod(concreteMethod, receiver) match {
            case Some(callee) =>
              val inc   = new IMethodInfo(callee)
              val pair  = new CallerCalleeInfo(caller, inc)

              if (pair isStampedForInlining info.stack) {
                retry = true
                if (isCountable)
                  count += 1

                pair.doInline(bb, i)
                inlinedMethodCount(inc.sym) += 1

                /* Remove this method from the cache, as the calls-private relation
                 * might have changed after the inlining.
                 */
                usesNonPublics -= m
              }
              else {
                if (settings.debug.value)
                  pair logFailure info.stack

                warnNoInline(pair failureReason info.stack)
              }
            case None =>
              warnNoInline("bytecode was not available")
              if (settings.debug.value)
                log("could not find icode\n\treceiver: " + receiver + "\n\tmethod: " + concreteMethod)
          }
        }
        else warnNoInline(
          if (!isAvailable) "bytecode was not available"
          else "it is not final"
        )
      }

      do {
        retry = false
        if (caller.inline) {
          log("Not inlining into " + caller.sym.originalName.decode + " because it is marked @inline.")
        }
        else if (caller.hasCode) {
          log("Analyzing " + m + " count " + count + " with " + caller.length + " blocks")
          tfa init m
          tfa.run
          caller.linearized foreach { bb =>
            info = tfa in bb

            for (i <- bb) {
              if (!retry) {
                i match {
                  case CALL_METHOD(msym, Dynamic) => analyzeInc(msym, i, bb)
                  case _                          => ()
                }
                info = tfa.interpret(info, i)
              }
            }
          }

          if (tfa.stat)
            log(m.symbol.fullName + " iterations: " + tfa.iterations + " (size: " + caller.length + ")")
        }
      }
      while (retry && count < MAX_INLINE_RETRY)

      m.normalize
		}

		private def isMonadicMethod(sym: Symbol) = sym.name match {
      case nme.foreach | nme.filter | nme.map | nme.flatMap => true
      case _                                                => false
    }
    private def isHigherOrderMethod(sym: Symbol) =
      sym.isMethod && atPhase(currentRun.erasurePhase.prev)(sym.info.paramTypes exists isFunctionType)

    /** Should method 'sym' being called in 'receiver' be loaded from disk? */
    def shouldLoadImplFor(sym: Symbol, receiver: Symbol): Boolean = {
      if (settings.debug.value)
        log("shouldLoadImplFor: " + receiver + "." + sym)

      def alwaysLoad    = (receiver.enclosingPackage == RuntimePackage) || (receiver == PredefModule.moduleClass)
      def loadCondition = sym.isEffectivelyFinal && isMonadicMethod(sym) && isHigherOrderMethod(sym)

      hasInline(sym) || alwaysLoad || loadCondition
    }

		/** Look up implementation of method 'sym in 'clazz'.
		 */
    def lookupImplFor(sym: Symbol, clazz: Symbol): Symbol = {
      // TODO: verify that clazz.superClass is equivalent here to clazz.tpe.parents(0).typeSymbol (.tpe vs .info)
      def needsLookup = (clazz != NoSymbol) && (clazz != sym.owner) && !sym.isEffectivelyFinal && clazz.isFinal

      def lookup(clazz: Symbol): Symbol = {
        // println("\t\tlooking up " + meth + " in " + clazz.fullName + " meth.owner = " + meth.owner)
        if (sym.owner == clazz || isBottomType(clazz)) sym
        else sym.overridingSymbol(clazz) match {
          case NoSymbol  => if (sym.owner.isTrait) sym else lookup(clazz.superClass)
          case imp       => imp
        }
      }
      if (needsLookup) {
        val concreteMethod = lookup(clazz)
        if (settings.debug.value)
          log("\tlooked up method: " + concreteMethod.fullName)

        concreteMethod
      }
      else sym
    }

    class IMethodInfo(val m: IMethod) {
      val sym           = m.symbol
      val name          = sym.name
      def owner         = sym.owner
      def paramTypes    = sym.info.paramTypes
      def minimumStack  = paramTypes.length + 1

      def inline        = hasInline(sym)
      def noinline      = hasNoInline(sym)
      def numInlined    = inlinedMethodCount(sym)

      def isBridge      = sym.isBridge
      def isInClosure   = isClosureClass(owner)
      def isHigherOrder = isHigherOrderMethod(sym)
      def isMonadic     = isMonadicMethod(sym)

      def handlers      = m.exh
      def blocks        = m.code.blocks
      def locals        = m.locals
      def length        = blocks.length
      def openBlocks    = blocks filterNot (_.closed)
      def instructions  = blocks.flatten
      def linearized    = linearizer linearize m

      def isSmall       = length <= SMALL_METHOD_SIZE
      def isLarge       = length > MAX_INLINE_SIZE
      def isRecursive   = m.recursive
      def hasCode       = m.code != null
      def hasSourceFile = m.sourceFile != null
      def hasHandlers   = handlers.nonEmpty

      def addLocals(ls: List[Local])  = m.locals ++= ls
      def addLocal(l: Local)          = addLocals(List(l))
      def addHandlers(exhs: List[ExceptionHandler]) = m.exh = exhs ::: m.exh
    }

    class CallerCalleeInfo(val caller: IMethodInfo, val inc: IMethodInfo) {
      def isLargeSum  = caller.length + inc.length - 1 > SMALL_METHOD_SIZE


      /** Inline 'inc' into 'caller' at the given block and instruction.
       *  The instruction must be a CALL_METHOD.
       */
      def doInline(block: BasicBlock, instr: Instruction) {
        val targetPos = instr.pos
        log("Inlining " + inc.m + " in " + caller.m + " at pos: " + posToStr(targetPos))

        def blockEmit(i: Instruction) = block.emit(i, targetPos)
        def newLocal(baseName: String, kind: TypeKind) =
          new Local(caller.sym.newVariable(targetPos, freshName(baseName)), kind, false)

        val a = new analysis.MethodTFA(inc.m)

        /* The exception handlers that are active at the current block. */
        val activeHandlers = caller.handlers filter (_ covered block)

        /* Map 'original' blocks to the ones inlined in the caller. */
        val inlinedBlock: mutable.Map[BasicBlock, BasicBlock] = new HashMap

        val varsInScope: mutable.Set[Local] = HashSet() ++= block.varsInScope

        /** Side effects varsInScope when it sees SCOPE_ENTERs. */
        def instrBeforeFilter(i: Instruction): Boolean = {
          i match { case SCOPE_ENTER(l) => varsInScope += l ; case _ => () }
          i ne instr
        }
        val instrBefore = block.toList takeWhile instrBeforeFilter
        val instrAfter  = block.toList drop (instrBefore.length + 1)

        assert(!instrAfter.isEmpty, "CALL_METHOD cannot be the last instruction in block!")

        // store the '$this' into the special local
        val inlinedThis = newLocal("$inlThis", REFERENCE(ObjectClass))

        /** buffer for the returned value */
        val retVal = inc.m.returnType match {
          case UNIT  => null
          case x     => newLocal("$retVal", x)
        }

        val inlinedLocals: mutable.Map[Local, Local] = new HashMap

        /** Add a new block in the current context. */
        def newBlock() = {
          val b = caller.m.code.newBlock
          activeHandlers foreach (_ addCoveredBlock b)
          if (retVal ne null) b.varsInScope += retVal
          b.varsInScope += inlinedThis
          b.varsInScope ++= varsInScope
          b
        }

        def translateExh(e: ExceptionHandler) = {
          val handler: ExceptionHandler = e.dup
          handler.covered = handler.covered map inlinedBlock
          handler setStartBlock inlinedBlock(e.startBlock)
          handler
        }

        /** alfa-rename `l' in caller's context. */
        def dupLocal(l: Local): Local = {
          val sym = caller.sym.newVariable(l.sym.pos, freshName(l.sym.name.toString()))
          // sym.setInfo(l.sym.tpe)
          val dupped = new Local(sym, l.kind, false)
          inlinedLocals(l) = dupped
          dupped
        }

        val afterBlock = newBlock()

        /** Map from nw.init instructions to their matching NEW call */
        val pending: mutable.Map[Instruction, NEW] = new HashMap

        /** Map an instruction from the callee to one suitable for the caller. */
        def map(i: Instruction): Instruction = {
          def assertLocal(l: Local) = {
            assert(caller.locals contains l, "Could not find local '" + l + "' in locals, nor in inlinedLocals: " + inlinedLocals)
            i
          }
          def isInlined(l: Local) = inlinedLocals isDefinedAt l

          val newInstr = i match {
            case THIS(clasz) =>
              LOAD_LOCAL(inlinedThis)

            case STORE_THIS(_) =>
              STORE_LOCAL(inlinedThis)

            case JUMP(whereto) =>
               JUMP(inlinedBlock(whereto))

            case CJUMP(success, failure, cond, kind) =>
               CJUMP(inlinedBlock(success), inlinedBlock(failure), cond, kind)

            case CZJUMP(success, failure, cond, kind) =>
              CZJUMP(inlinedBlock(success), inlinedBlock(failure), cond, kind)

            case SWITCH(tags, labels) =>
              SWITCH(tags, labels map inlinedBlock)

            case RETURN(kind) =>
              JUMP(afterBlock)

            case LOAD_LOCAL(l) if isInlined(l) =>
              LOAD_LOCAL(inlinedLocals(l))

            case STORE_LOCAL(l) if isInlined(l) =>
              STORE_LOCAL(inlinedLocals(l))

            case LOAD_LOCAL(l)   => assertLocal(l)
            case STORE_LOCAL(l)  => assertLocal(l)

            case SCOPE_ENTER(l) if isInlined(l) =>
              SCOPE_ENTER(inlinedLocals(l))

            case SCOPE_EXIT(l) if isInlined(l) =>
              SCOPE_EXIT(inlinedLocals(l))

            case nw @ NEW(sym) =>
              val r = NEW(sym)
              pending(nw.init) = r
              r

            case CALL_METHOD(meth, Static(true)) if meth.isClassConstructor =>
              CALL_METHOD(meth, Static(true))

            case _ => i.clone()
          }
          // check any pending NEW's
          pending remove i foreach (_.init = newInstr.asInstanceOf[CALL_METHOD])
          newInstr
        }

        caller addLocals (inc.locals map dupLocal)
        caller addLocal inlinedThis

        if (retVal ne null)
          caller addLocal retVal

        inc.blocks foreach { b =>
          inlinedBlock += (b -> newBlock())
          inlinedBlock(b).varsInScope ++= (b.varsInScope map inlinedLocals)
        }

        // analyse callee
        a.run

        // re-emit the instructions before the call
        block.open
        block.clear
        block emit instrBefore

        // store the arguments into special locals
        inc.m.params.reverse foreach (p => blockEmit(STORE_LOCAL(inlinedLocals(p))))
        blockEmit(STORE_LOCAL(inlinedThis))

        // jump to the start block of the callee
        blockEmit(JUMP(inlinedBlock(inc.m.code.startBlock)))
        block.close

        // duplicate the other blocks in the callee
        linearizer linearize inc.m foreach { bb =>
          var info = a in bb
          def emitInlined(i: Instruction) = inlinedBlock(bb).emit(i, targetPos)
          def emitDrops(toDrop: Int)      = info.stack.types drop toDrop foreach (t => emitInlined(DROP(t)))

          for (i <- bb) {
            i match {
              case RETURN(UNIT) => emitDrops(0)
              case RETURN(kind) =>
                if (info.stack.length > 1) {
                  emitInlined(STORE_LOCAL(retVal))
                  emitDrops(1)
                  emitInlined(LOAD_LOCAL(retVal))
                }
              case _            => ()
            }
            emitInlined(map(i))
            info = a.interpret(info, i)
          }
          inlinedBlock(bb).close
        }

        afterBlock emit instrAfter
        afterBlock.close
        count += 1

        // add exception handlers of the callee
        caller addHandlers (inc.handlers map translateExh)
        assert(pending.isEmpty, "Pending NEW elements: " + pending)
      }

      def isStampedForInlining(stack: TypeStack) =
        !sameSymbols && inc.hasCode && shouldInline && isSafeToInline(stack)

      def logFailure(stack: TypeStack) = log(
        """|inline failed for %s:
           |  pair.sameSymbols: %s
           |  inc.numInlined < 2: %s
           |  inc.hasCode: %s
           |  isSafeToInline: %s
           |  shouldInline: %s
        """.stripMargin.format(
          inc.m, sameSymbols, inc.numInlined < 2,
          inc.hasCode, isSafeToInline(stack), shouldInline
        )
      )

      def failureReason(stack: TypeStack) =
        if (!inc.hasCode) "bytecode was unavailable"
        else if (!isSafeToInline(stack)) "it is unsafe (target may reference private fields)"
        else "of a bug (run with -Ylog:inline -Ydebug for more information)"

      def canAccess(level: NonPublicRefs.Value) = level match {
        case Private    => caller.owner == inc.owner
        case Protected  => caller.owner.tpe <:< inc.owner.tpe
        case Public     => true
      }
      private def sameSymbols = caller.sym == inc.sym

      /** A method is safe to inline when:
       *    - it does not contain calls to private methods when
       *      called from another class
       *    - it is not inlined into a position with non-empty stack,
       *      while having a top-level finalizer (see liftedTry problem)
       *    - it is not recursive
       * Note:
       *    - synthetic private members are made public in this pass.
       */
      def isSafeToInline(stack: TypeStack): Boolean = {
        def makePublic(f: Symbol): Boolean =
          inc.hasSourceFile && (f.isSynthetic || f.isParamAccessor) && {
            if (settings.debug.value)
              log("Making not-private symbol out of synthetic: " + f)

            f setFlag Flags.notPRIVATE
            true
          }

        if (inc.isRecursive)
          return false

        val accessNeeded = usesNonPublics.getOrElseUpdate(inc.m, {
          // Avoiding crashing the compiler if there are open blocks.
          inc.openBlocks foreach { b =>
            warn(inc.sym.pos,
                "Encountered open block in isSafeToInline: this indicates a bug in the optimizer!\n" +
                "  caller = " + caller.m + ", callee = " + inc.m
              )
            return false
          }
          def check(sym: Symbol, cond: Boolean) =
            if (cond) Private
            else if (sym.isProtected) Protected
            else Public

          def checkField(f: Symbol)   = check(f, f.isPrivate && !makePublic(f))
          def checkSuper(m: Symbol)   = check(m, m.isPrivate || !m.isClassConstructor)
          def checkMethod(m: Symbol)  = check(m, m.isPrivate)

          def getAccess(i: Instruction) = i match {
            case CALL_METHOD(m, SuperCall(_)) => checkSuper(m)
            case CALL_METHOD(m, _)            => checkMethod(m)
            case LOAD_FIELD(f, _)             => checkField(f)
            case STORE_FIELD(f, _)            => checkField(f)
            case _                            => Public
          }

          def iterate(): NonPublicRefs.Value = {
            var seenProtected = false
            inc.instructions foreach { i =>
              getAccess(i) match {
                case Private    => return Private
                case Protected  => seenProtected = true
                case _          => ()
              }
            }
            if (seenProtected) Protected else Public
          }
          iterate()
        })

        def isIllegalStack = (stack.length > inc.minimumStack && inc.hasHandlers) || {
          if (settings.debug.value)
            log("method " + inc.sym + " is used on a non-empty stack with finalizer.")

          false
        }

        canAccess(accessNeeded) && !isIllegalStack
      }

      /** Decide whether to inline or not. Heuristics:
       *   - it's bad to make the caller larger (> SMALL_METHOD_SIZE) if it was small
       *   - it's bad to inline large methods
       *   - it's good to inline higher order functions
       *   - it's good to inline closures functions.
       *   - it's bad (useless) to inline inside bridge methods
       */
      private def neverInline   = caller.isBridge || inc.noinline
      private def alwaysInline  = inc.inline

      def shouldInline: Boolean = !neverInline && (alwaysInline || {
        if (settings.debug.value)
          log("shouldInline: " + caller.m + " with " + inc.m)

        var score = 0
        if (inc.isSmall)
          score += 1
        if (caller.isSmall && isLargeSum) {
          score -= 1
          if (settings.debug.value)
            log("shouldInline: score decreased to " + score + " because small " + caller + " would become large")
        }
        if (inc.isLarge)
          score -= 1

        if (inc.isMonadic)
          score += 2
        else if (inc.isHigherOrder)
          score += 1
        if (inc.isInClosure)
          score += 2
        if (inc.numInlined > 2)
          score -= 2

        if (settings.debug.value)
          log("shouldInline(" + inc.m + ") score: " + score)

        score > 0
      })
    }

    def lookupIMethod(meth: Symbol, receiver: Symbol): Option[IMethod] = {
      def tryParent(sym: Symbol) = icodes icode sym flatMap (_ lookupMethod meth)

      receiver.info.baseClasses.iterator map tryParent find (_.isDefined) getOrElse None
    }
  } /* class Inliner */
} /* class Inliners */
