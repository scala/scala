/* NSC -- new scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Iulian Dragos
 */


package scala.tools.nsc
package backend.opt

import scala.collection.{ mutable, immutable }

/**
 */
abstract class DeadCodeElimination extends SubComponent {
  import global._
  import icodes._
  import icodes.opcodes._
  import definitions.RuntimePackage

  /** The block and index where an instruction is located */
  type InstrLoc = (BasicBlock, Int)

  val phaseName = "dce"

  override val enabled: Boolean = settings.Xdce

  /** Create a new phase */
  override def newPhase(p: Phase) = new DeadCodeEliminationPhase(p)

  /** Dead code elimination phase.
   */
  class DeadCodeEliminationPhase(prev: Phase) extends ICodePhase(prev) {

    def name = phaseName
    val dce = new DeadCode()

    override def apply(c: IClass) {
      if (settings.Xdce && (dce ne null))
        dce.analyzeClass(c)
    }
  }

  /** closures that are instantiated at least once, after dead code elimination */
  val liveClosures = perRunCaches.newSet[Symbol]()

  /** closures that are eliminated, populated by GenASM.AsmPhase.run()
   *  these class symbols won't have a .class physical file, thus shouldn't be included in InnerClasses JVM attribute,
   *  otherwise some tools get confused or slow (SI-6546)
   * */
  val elidedClosures = perRunCaches.newSet[Symbol]()

  /** Remove dead code.
   */
  class DeadCode {

    def analyzeClass(cls: IClass) {
      log(s"Analyzing ${cls.methods.size} methods in $cls.")
      cls.methods.foreach { m =>
        this.method = m
        dieCodeDie(m)
        global.closureElimination.peephole(m)
      }
    }

    val rdef = new reachingDefinitions.ReachingDefinitionsAnalysis

    /** Use-def chain: give the reaching definitions at the beginning of given instruction. */
    var defs: immutable.Map[InstrLoc, immutable.Set[rdef.lattice.Definition]] = immutable.HashMap.empty

    /** Useful instructions which have not been scanned yet. */
    val worklist: mutable.Set[InstrLoc] = new mutable.LinkedHashSet

    /** what instructions have been marked as useful? */
    val useful: mutable.Map[BasicBlock, mutable.BitSet] = perRunCaches.newMap()

    /** what local variables have been accessed at least once? */
    var accessedLocals: List[Local] = Nil

    /** Map from a local and a basic block to the instructions that store to that local in that basic block */
    val localStores = mutable.Map[(Local, BasicBlock), mutable.BitSet]() withDefault {_ => mutable.BitSet()}

    /** Stores that clobber previous stores to array or ref locals. See SI-5313 */
    val clobbers = mutable.Set[InstrLoc]()

    /** the current method. */
    var method: IMethod = _

    /** Map instructions who have a drop on some control path, to that DROP instruction. */
    val dropOf: mutable.Map[InstrLoc, List[InstrLoc]] = perRunCaches.newMap()

    def dieCodeDie(m: IMethod) {
      if (m.hasCode) {
        debuglog("dead code elimination on " + m)
        dropOf.clear()
        localStores.clear()
        clobbers.clear()
        m.code.blocks.clear()
        m.code.touched = true
        accessedLocals = m.params.reverse
        m.code.blocks ++= linearizer.linearize(m)
        m.code.touched = true
        collectRDef(m)
        mark()
        sweep(m)
        accessedLocals = accessedLocals.distinct
        val diff = m.locals diff accessedLocals
        if (diff.nonEmpty) {
          val msg = diff.map(_.sym.name)mkString(", ")
          log(s"Removed ${diff.size} dead locals: $msg")
          m.locals = accessedLocals.reverse
        }
      }
    }

    /** collect reaching definitions and initial useful instructions for this method. */
    def collectRDef(m: IMethod): Unit = if (m.hasCode) {
      defs = immutable.HashMap.empty; worklist.clear(); useful.clear()
      rdef.init(m)
      rdef.run()

      m foreachBlock { bb =>
        useful(bb) = new mutable.BitSet(bb.size)
        var rd = rdef.in(bb)
        for ((i, idx) <- bb.toList.zipWithIndex) {

          // utility for adding to worklist
          def moveToWorkList() = moveToWorkListIf(cond = true)

          // utility for (conditionally) adding to worklist
          def moveToWorkListIf(cond: Boolean) =
            if (cond) {
              debuglog("in worklist:     " + i)
              worklist += ((bb, idx))
            } else {
              debuglog("not in worklist: " + i)
            }

          // instruction-specific logic
          i match {

            case LOAD_LOCAL(_) =>
              defs = defs + (((bb, idx), rd.vars))
              moveToWorkListIf(cond = false)

            case STORE_LOCAL(l) =>
              /* SI-4935 Check whether a module is stack top, if so mark the instruction that loaded it
               * (otherwise any side-effects of the module's constructor go lost).
               *   (a) The other two cases where a module's value is stored (STORE_FIELD and STORE_ARRAY_ITEM)
               *       are already marked (case clause below).
               *   (b) A CALL_METHOD targeting a method `m1` where the receiver is potentially a module (case clause below)
               *       will have the module's load marked provided `isSideEffecting(m1)`.
               *       TODO check for purity (the ICode?) of the module's constructor (besides m1's purity).
               *       See also https://github.com/paulp/scala/blob/topic/purity-analysis/src/compiler/scala/tools/nsc/backend/opt/DeadCodeElimination.scala
               */
              val necessary = rdef.findDefs(bb, idx, 1) exists { p =>
                val (bb1, idx1) = p
                bb1(idx1) match {
                  case LOAD_MODULE(module) => isLoadNeeded(module)
                  case _                   => false
                }
              }
              moveToWorkListIf(necessary)

              // add it to the localStores map
              val key = (l, bb)
              val set = localStores(key)
              set += idx
              localStores(key) = set

            case RETURN(_) | JUMP(_) | CJUMP(_, _, _, _) | CZJUMP(_, _, _, _) | STORE_FIELD(_, _) |
                 THROW(_)   | LOAD_ARRAY_ITEM(_) | STORE_ARRAY_ITEM(_) | SCOPE_ENTER(_) | SCOPE_EXIT(_) | STORE_THIS(_) |
                 LOAD_EXCEPTION(_) | SWITCH(_, _) | MONITOR_ENTER() | MONITOR_EXIT() | CHECK_CAST(_) | CREATE_ARRAY(_, _) =>
              moveToWorkList()

            case LOAD_FIELD(sym, isStatic) if isStatic || !inliner.isClosureClass(sym.owner) =>
              // static load may trigger static initialization.
              // non-static load can throw NPE (but we know closure fields can't be accessed via a
              // null reference.
              moveToWorkList()
            case CALL_METHOD(m1, _) if isSideEffecting(m1) =>
              moveToWorkList()

            case CALL_METHOD(m1, SuperCall(_)) =>
              moveToWorkList() // super calls to constructor

            case DROP(_) =>
              val necessary = rdef.findDefs(bb, idx, 1) exists { p =>
                val (bb1, idx1) = p
                bb1(idx1) match {
                  case CALL_METHOD(m1, _) if isSideEffecting(m1) => true
                  case LOAD_EXCEPTION(_) | DUP(_) | LOAD_MODULE(_) => true
                  case _ =>
                    dropOf((bb1, idx1)) = (bb,idx) :: dropOf.getOrElse((bb1, idx1), Nil)
                    debuglog("DROP is inessential: " + i + " because of: " + bb1(idx1) + " at " + bb1 + ":" + idx1)
                    false
                }
              }
              moveToWorkListIf(necessary)
            case LOAD_MODULE(sym) if isLoadNeeded(sym) =>
              moveToWorkList() // SI-4859 Module initialization might side-effect.
            case CALL_PRIMITIVE(Arithmetic(DIV | REM, INT | LONG) | ArrayLength(_)) =>
              moveToWorkList() // SI-8601 Might divide by zero
            case _ => ()
              moveToWorkListIf(cond = false)
          }
          rd = rdef.interpret(bb, idx, rd)
        }
      }
    }

    private def isLoadNeeded(module: Symbol): Boolean = {
      module.info.member(nme.CONSTRUCTOR).filter(isSideEffecting) != NoSymbol
    }

    /** Mark useful instructions. Instructions in the worklist are each inspected and their
     *  dependencies are marked useful too, and added to the worklist.
     */
    def mark() {
//      log("Starting with worklist: " + worklist)
      while (!worklist.isEmpty) {
        val (bb, idx) = worklist.head
        worklist -= ((bb, idx))
        debuglog("Marking instr: \tBB_" + bb + ": " + idx + " " + bb(idx))

        val instr = bb(idx)
        // adds the instructions that define the stack values about to be consumed to the work list to
        // be marked useful
        def addDefs() = for ((bb1, idx1) <- rdef.findDefs(bb, idx, instr.consumed) if !useful(bb1)(idx1)) {
          debuglog(s"\t${bb1(idx1)} is consumed by $instr")
          worklist += ((bb1, idx1))
        }

        // DROP logic -- if an instruction is useful, its drops are also useful
        // and we don't mark the DROPs as useful directly but add them to the
        // worklist so we also mark their reaching defs as useful - see SI-7060
        if (!useful(bb)(idx)) {
          useful(bb) += idx
          dropOf.get((bb, idx)) foreach {
            for ((bb1, idx1) <- _) {
              /*
               * SI-7060: A drop that we now mark as useful can be reached via several paths,
               * so we should follow by marking all its reaching definition as useful too:
               */
              debuglog("\tAdding: " + bb1(idx1) + " to the worklist, as a useful DROP.")
              worklist += ((bb1, idx1))
            }
          }

          // per-instruction logic
          instr match {
            case LOAD_LOCAL(l1) =>
              for ((l2, bb1, idx1) <- defs((bb, idx)) if l1 == l2; if !useful(bb1)(idx1)) {
                debuglog("\tAdding " + bb1(idx1))
                worklist += ((bb1, idx1))
              }

            case STORE_LOCAL(l1) if l1.kind.isRefOrArrayType =>
              addDefs()
              // see SI-5313
              // search for clobbers of this store if we aren't doing l1 = null
              // this doesn't catch the second store in x=null;l1=x; but in practice this catches
              // a lot of null stores very cheaply
              if (idx == 0 || bb(idx - 1) != CONSTANT(Constant(null)))
                 findClobbers(l1, bb, idx + 1)

            case nw @ NEW(REFERENCE(sym)) =>
              assert(nw.init ne null, "null new.init at: " + bb + ": " + idx + "(" + instr + ")")
              worklist += findInstruction(bb, nw.init)
              if (inliner.isClosureClass(sym)) {
                liveClosures += sym
              }

            // it may be better to move static initializers from closures to
            // the enclosing class, to allow the optimizer to remove more closures.
            // right now, the only static fields in closures are created when caching
            // 'symbol literals.
            case LOAD_FIELD(sym, true) if inliner.isClosureClass(sym.owner) =>
              log("added closure class for field " + sym)
              liveClosures += sym.owner

            case LOAD_EXCEPTION(_) =>
              ()

            case _ =>
              addDefs()
          }
        }
      }
    }

    /**
     * Finds and marks all clobbers of the given local starting in the given
     * basic block at the given index
     *
     * Storing to local variables of reference or array type may be indirectly
     * observable because it may remove a reference to an object which may allow the object
     * to be gc'd. See SI-5313. In this code I call the LOCAL_STORE(s) that immediately follow a
     * LOCAL_STORE and that store to the same local "clobbers." If a LOCAL_STORE is marked
     * useful then its clobbers must go into the set of clobbers, which will be
     * compensated for later
     */
    def findClobbers(l: Local, bb: BasicBlock, idx: Int) {
        // previously visited blocks tracked to prevent searching forever in a cycle
        val inspected = mutable.Set[BasicBlock]()
        // our worklist of blocks that still need to be checked
        val blocksToBeInspected = mutable.Set[BasicBlock]()

        // Tries to find the next clobber of l1 in bb1 starting at idx1.
        // if it finds one it adds the clobber to clobbers set for later
        // handling. If not it adds the direct successor blocks to
        // the uninspectedBlocks to try to find clobbers there. Either way
        // it adds the exception successor blocks for further search
        def findClobberInBlock(idx1: Int, bb1: BasicBlock) {
          val key = ((l, bb1))
          val foundClobber = (localStores contains key) && {
            def minIdx(s : mutable.BitSet) = if(s.isEmpty) -1 else s.min

            // find the smallest index greater than or equal to idx1
            val clobberIdx = minIdx(localStores(key) dropWhile (_ < idx1))
            if (clobberIdx == -1)
              false
            else {
              debuglog(s"\t${bb1(clobberIdx)} is a clobber of ${bb(idx)}")
              clobbers += ((bb1, clobberIdx))
              true
            }
          }

          // always need to look into the exception successors for additional clobbers
          // because we don't know when flow might enter an exception handler
          blocksToBeInspected ++= (bb1.exceptionSuccessors filterNot inspected)
          // If we didn't find a clobber here then we need to look at successor blocks.
          // if we found a clobber then we don't need to search in the direct successors
          if (!foundClobber) {
            blocksToBeInspected ++= (bb1.directSuccessors filterNot inspected)
          }
        }

        // first search starting at the current index
        // note we don't put bb in the inspected list yet because a loop may later force
        // us back around to search from the beginning of bb
        findClobberInBlock(idx, bb)
        // then loop until we've exhausted the set of uninspected blocks
        while(!blocksToBeInspected.isEmpty) {
          val bb1 = blocksToBeInspected.head
          blocksToBeInspected -= bb1
          inspected += bb1
          findClobberInBlock(0, bb1)
        }
    }

    def sweep(m: IMethod) {
      val compensations = computeCompensations(m)

      debuglog("Sweeping: " + m)

      m foreachBlock { bb =>
        debuglog(bb + ":")
        val oldInstr = bb.toList
        bb.open()
        bb.clear()
        for ((i, idx) <- oldInstr.zipWithIndex) {
          if (useful(bb)(idx)) {
            debuglog(" * " + i + " is useful")
            bb.emit(i, i.pos)
            compensations.get((bb, idx)) match {
              case Some(is) => is foreach bb.emit
              case None => ()
            }
            // check for accessed locals
            i match {
              case LOAD_LOCAL(l) if !l.arg =>
                accessedLocals = l :: accessedLocals
              case STORE_LOCAL(l) if !l.arg =>
                accessedLocals = l :: accessedLocals
              case _ => ()
            }
          } else {
            i match {
              case NEW(REFERENCE(sym)) =>
                log(s"Eliminated instantiation of $sym inside $m")
              case STORE_LOCAL(l) if clobbers contains ((bb, idx)) =>
                // if an unused instruction was a clobber of a used store to a reference or array type
                // then we'll replace it with the store of a null to make sure the reference is
                // eliminated. See SI-5313
                bb emit CONSTANT(Constant(null))
                bb emit STORE_LOCAL(l)
              case _ => ()
            }
            debuglog("   " + i + " [swept]")
          }
        }

        if (bb.nonEmpty) bb.close()
        else log(s"empty block encountered in $m")
      }
    }

    private def computeCompensations(m: IMethod): mutable.Map[InstrLoc, List[Instruction]] = {
      val compensations: mutable.Map[InstrLoc, List[Instruction]] = new mutable.HashMap

      m foreachBlock { bb =>
        assert(bb.closed, "Open block in computeCompensations")
        foreachWithIndex(bb.toList) { (i, idx) =>
          if (!useful(bb)(idx)) {
            foreachWithIndex(i.consumedTypes.reverse) { (consumedType, depth) =>
              debuglog("Finding definitions of: " + i + "\n\t" + consumedType + " at depth: " + depth)
              val defs = rdef.findDefs(bb, idx, 1, depth)
              for (d <- defs) {
                val (bb, idx) = d
                debuglog("rdef: "+ bb(idx))
                bb(idx) match {
                  case DUP(_) if idx > 0 =>
                    bb(idx - 1) match {
                      case nw @ NEW(_) =>
                        val init = findInstruction(bb, nw.init)
                        log("Moving DROP to after <init> call: " + nw.init)
                        compensations(init) = List(DROP(consumedType))
                      case _ =>
                        compensations(d) = List(DROP(consumedType))
                    }
                  case _ =>
                    compensations(d) = List(DROP(consumedType))
                }
              }
            }
          }
        }
      }
      compensations
    }

    private def findInstruction(bb: BasicBlock, i: Instruction): InstrLoc = {
      for (b <- linearizer.linearizeAt(method, bb)) {
        val idx = b.toList indexWhere (_ eq i)
        if (idx != -1)
          return (b, idx)
      }
      abort("could not find init in: " + method)
    }

    private def isPure(sym: Symbol) = (
         (sym.isGetter && sym.isEffectivelyFinalOrNotOverridden && !sym.isLazy)
      || (sym.isPrimaryConstructor && (sym.enclosingPackage == RuntimePackage || inliner.isClosureClass(sym.owner)))
    )
    /** Is 'sym' a side-effecting method? TODO: proper analysis.  */
    private def isSideEffecting(sym: Symbol) = !isPure(sym)

  } /* DeadCode */
}
