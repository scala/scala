/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package backend.jvm
package opt

import scala.annotation.{tailrec, unused}
import scala.collection.AbstractIterator
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.tools.asm.Opcodes._
import scala.tools.asm.Type
import scala.tools.asm.tree._
import scala.tools.nsc.backend.jvm.BTypes.InternalName
import scala.tools.nsc.backend.jvm.analysis.{AsmAnalyzer, BackendUtils, ProdConsAnalyzer}
import scala.tools.nsc.backend.jvm.opt.BytecodeUtils._

abstract class BoxUnbox {
  val postProcessor: PostProcessor

  import postProcessor.{backendUtils, callGraph}
  import backendUtils._

  /**
   * Eliminate box-unbox pairs within `method`. Such appear commonly after closure elimination:
   *
   *   def t2 = {
   *     val f = (b: Byte, i: Int) => i + b // no specialized variant for this function type
   *     f(1, 2) // invokes the generic `apply`
   *   }
   *
   * The closure optimizer re-writes the `apply` call to `anonfun\$adapted` method, which takes
   * boxed arguments. After inlining this method, we get
   *
   *   def t2 = {
   *     val a = boxByte(1)
   *     val b = boxInteger(2)
   *     val r = boxInteger(anonfun\$(unboxByte(a), unboxInt(b)))
   *     unboxInt(r)
   *   }
   *
   * All these box/unbox operations are eliminated here.
   *
   * Implementation: for every box operation, find all consumers of the boxed value, then all
   * producers of these consumers, repeat until reaching a fixpoint. If this results in a set of
   * boxing and unboxing operations, the box can be eliminated.
   *
   * There are two methods for eliminating boxes:
   *   M1: If there is a single boxing operation, the boxed value(s) are stored into new local
   *       variable(s) at the allocation site. Accesses to the boxed value are re-written to reads /
   *       writes of these locals. Advantages:
   *         - supports mutable boxes (IntRef and friends)
   *         - supports eliminating unbox operations even if the box object needs to be created
   *           because it escapes (see E4)
   *           - works by keeping the unboxed value(s) in locals AND the box in its original form
   *           - only for immutable boxes: modifications to the escaped box cannot be applied to
   *             the local variable(s) holding the boxed value(s).
   *       Restriction:
   *         - does not work if there are multiple boxing operations (see E1)
   *
   *   M2: If there are multiple boxing operations, the boxing operations are simply eliminated,
   *       leaving the unboxed value(s) on the stack. Store / load operations that previously
   *       acted on the box are adapted to handle the boxed type(s). If the box contains multiple
   *       values (or a size-2 value, which doesn't fit into locals that were used for the box),
   *       new local slots are used for store / load operations. Restrictions:
   *         - does not support re-writing writes to (mutable) boxes (see E2)
   *         - does not support re-writing reads of boxes that also escape (see E3)
   *
   *
   * E1: M1 only works if there's a single boxing operation.
   *   def e1(b: Boolean) = {
   *     val i: Integer = box(10) // 10 is stored into a new local, box operation and i removed
   *     val j: Integer = box(20) // 20 is stored into a new local, box operation and j removed
   *     val r = if (b) i else j  // loads and stores of the box are eliminated, r no longer exists
   *     unbox(r)                 // cannot rewrite: we don't know which local to load
   *   }
   * Note: the example has no write and the box does not escape, so M2 works here.
   *
   * E2: mutable boxes with multiple boxing operations cannot be eliminated.
   *   M1: see E1
   *   M2: cannot replace an `IntRef` on the stack by an `Int` value on the stack, an Int on the
   *       stack cannot be modified.
   *
   *   def e2(b: Boolean) = {
   *     val r1 = new IntRef(0)
   *     val r2 = new IntRef(1)
   *     val modRef = if (b) r1 else r2
   *     modRef.elem += 10               // M1: cannot rewrite: which local to write? same as E1.
   *     (if (b) r1 else r2).elem += 10  // M2: cannot change an Int on the stack
   *     (r1.elem, r2.elem)
   *   }
   *
   *
   * E3: escaping boxes with multiple boxing operations cannot be rewritten.
   *   M1: see E1.
   *   M2: at *, instead of an Integer, an Int is on the stack, but the escape method expects an
   *       Integer. We cannot just create a box at this point: if there are multiple escapes (or
   *       an escape is executed more than once), the difference could be observed (reference
   *       equality).
   *
   *   def e3(b: Boolean) = {
   *     val i: Integer = box(1)
   *     val j: Integer = box(2)
   *     escape(if (b) i else j)  // *
   *     unbox(if (b) i else j)
   *  }
   *
   *
   * E4: M1 supports rewriting unbox operations of immutable boxes that escape
   *   def e4 = {
   *     val i: Integer = box(10) // 10 is stored into a new local, loaded as argument for the box call
   *     escape(i)                // not changed, still loads the local i holding the box
   *     unbox(i)                 // rewritten to a pop (of the box) and a load of the local variable
   *   }
   *
   *
   * E4 seems to be a bit of a corner case, but it's necessary to unblock box eliminations with
   * mutual dependencies. Example:
   *
   *   val ((a, b), c) = ((1, 2), 3)
   *   a + b + c
   *
   * generates (after a few cleanups) the following (pseudo-bytecode, ignoring primitive boxing, specialization):
   *
   *   load 1, load 2, new Tuple2  // stack: Tuple2
   *   load 3                      // stack: Tuple2; Int
   *   val local1 = new Tuple2
   *   val local2 = local1._1.asInstanceOf[Tuple2]
   *   val c = local1._2.asInstanceOf[Int]
   *   if (local2 == null) throw new MatchError(local1)
   *   val a = local2._1
   *   val b = local2._2
   *   a + b + c
   *
   * In order to eliminate the tuples, we first need to eliminate the outer tuple (stored in local1)
   *   - single box operation, so we use M1
   *   - there are three consumers of the outer tuple: `local1._1`, `local1._2` and
   *     `new MatchError(local1)`. in the last one, the tuple escapes.
   *   - note that the MatchError creation is dead code: local2 is never null. However, our nullness
   *     analysis cannot identify this: it does not track nullness through tuple stores and loads.
   *   - if we re-write the non-escaping consumers of the outer tuple, but keep the tuple allocation
   *     and the escaping consumer, we get the following:
   *
   *   load 1, load 2
   *   val newLocal1 = new Tuple2; load newLocal1  // stack: Tuple2
   *   val newLocal2 = 3; load newLocal2           // stack: Tuple2; Int
   *   val local1 = new Tuple2
   *   val local2 = newLocal1
   *   val c = newLocal2
   *   if (local2 == null) throw new MatchError(local1)
   *   val a = local2._1
   *   val b = local2._2
   *   a + b + c
   *
   * At this point, the nullness analysis sees that `local2 == null` is false, dead code elimination
   * removes the `throw new MatchError(local1)`. After eliminating the allocation of the outer tuple,
   * the inner tuple (stored in newLocal1) can also be eliminated.
   *
   *
   * Special case for tuples wrt specialization: a tuple getter may box or unbox the value stored
   * in the tuple: calling `_1` on a `Tuple2\$mcII\$sp` boxes the primitive Int stored in the tuple.
   * Similarly, calling `_1\$mcI\$sp` on a non-specialized `Tuple2` unboxes the Integer in the tuple.
   * When eliminating such getters, we have to introduce appropriate box / unbox calls.
   *
   *
   * TODO: add new calls (box / unbox) to the call graph (not urgent)
   * TODO: update the call graph because stack heights change (not urgent).
   *   this may also affect other optimizations, we ignored the issue so far. check how stack
   *   heights stored in the call graph are used.
   * Note: these tasks are not urgent because the call graph is not currently used during / after
   * method-local optimizations, only before to perform inlining and closure rewriting.
   */
  def boxUnboxElimination(method: MethodNode, owner: InternalName): Boolean = {
    AsmAnalyzer.sizeOKForSourceValue(method) && {
      val toInsertBefore = mutable.Map.empty[AbstractInsnNode, List[AbstractInsnNode]]
      val toReplace = mutable.Map.empty[AbstractInsnNode, List[AbstractInsnNode]]
      val toDelete = mutable.Set.empty[AbstractInsnNode]

      val knownHandled = mutable.Set.empty[AbstractInsnNode]

      lazy val prodCons = new ProdConsAnalyzer(method, owner)

      var nextLocal = BackendUtils.maxLocals(method)
      def getLocal(size: Int) = {
        val r = nextLocal
        nextLocal += size
        r
      }

      var maxStackGrowth = 0

      /* Method M1 for eliminating box-unbox pairs (see doc comment in the beginning of this file) */
      def replaceBoxOperationsSingleCreation(creation: BoxCreation, finalCons: Set[BoxConsumer], boxKind: BoxKind, keepBox: Boolean): Unit = {
        /*
         * If the box is eliminated, all copy operations (loads, stores, others) of the box need to
         * be removed. This method returns all copy operations that should be removed.
         *
         * Returns `None` in case some exotic copy operation is found that cannot be removed
         * (DUP2_X1 and friends - these are never emitted by scalac). In this case, the box cannot
         * be eliminated.
         */
        def copyOpsToEliminate: Option[Set[AbstractInsnNode]] = {
          var elidableCopyOps = Set.empty[AbstractInsnNode]
          var replaceOK = true
          val copyOps = new CopyOpsIterator(Set(creation), finalCons, prodCons)
          while (replaceOK && copyOps.hasNext) copyOps.next() match {
            case vi: VarInsnNode =>
              elidableCopyOps += vi

            case copyOp if copyOp.getOpcode == DUP =>
              elidableCopyOps += copyOp

            case _ =>
              replaceOK = false
          }
          if (replaceOK) Some(elidableCopyOps) else None
        }

        val canRewrite = keepBox || (copyOpsToEliminate match {
          case Some(copyOps) =>
            toDelete ++= copyOps
            true

          case _ => false
        })

        if (canRewrite) {
          val localSlots = Vector.from[(Int, Type)](boxKind.boxedTypes.iterator.map(tp => (getLocal(tp.getSize), tp)))

          // store boxed value(s) into localSlots
          val storeOps = localSlots.reverseIterator map { case (slot, tp) =>
            new VarInsnNode(tp.getOpcode(ISTORE), slot)
          } to(List)
          val storeInitialValues = creation.loadInitialValues match {
            case Some(ops) => ops ::: storeOps
            case None => storeOps
          }
          if (keepBox) {
            val loadOps = List.from[VarInsnNode](localSlots.iterator.map({ case (slot, tp) =>
              new VarInsnNode(tp.getOpcode(ILOAD), slot)
            }))
            toInsertBefore(creation.valuesConsumer) = storeInitialValues ::: loadOps
          } else {
            toReplace(creation.valuesConsumer) = storeInitialValues
            toDelete ++= creation.allInsns - creation.valuesConsumer
          }

          // rewrite consumers
          finalCons foreach {
            case write: StaticSetterOrInstanceWrite =>
              assert(!keepBox, s"cannot eliminate box write if the box remains (and escapes): $write")
              val (slot, tp) = localSlots(boxKind.extractedValueIndex(write))
              val storeOp = new VarInsnNode(tp.getOpcode(ISTORE), slot)
              toReplace(write.consumer) = List(storeOp)

            case c: EscapingConsumer =>
              assert(keepBox, s"found escaping consumer, but box is eliminated: $c")

            case Drop(insn) =>
              if (keepBox) toReplace(insn) = List(getPop(1))
              else toDelete += insn

            case extraction =>
              val (slot, tp) = localSlots(boxKind.extractedValueIndex(extraction))
              val loadOps = new VarInsnNode(tp.getOpcode(ILOAD), slot) :: extraction.postExtractionAdaptationOps(tp)
              if (keepBox) toReplace(extraction.consumer) = getPop(1) :: loadOps
              else toReplace(extraction.consumer) = loadOps
              toDelete ++= extraction.allInsns - extraction.consumer
          }
        }
      }

      /* Method M2 for eliminating box-unbox pairs (see doc comment in the beginning of this file) */
      def replaceBoxOperationsMultipleCreations(allCreations: Set[BoxCreation], allConsumers: Set[BoxConsumer], boxKind: BoxKind): Unit = {
        /*
         * If a single-value size-1 box is eliminated, local variables slots holding the box are
         * reused to hold the unboxed value. In case there's an entry for that local variable in the
         * method's local variables table (debug info), adapt the type.
         *
         * If there are multiple entries for a local variable that's changing types, then all
         * entries for that variable are deleted - it's not obvious how to find the correct entry.
         * Note that scalac never re-uses local variable slots for non-overlapping locals. Also note
         * that all locals that are newly created during the optimizer don't have an entry either.
         *
         * Finally, note that variables that become unused are removed later from the table by
         * removeUnusedLocalVariableNodes in LocalOpt.
         *
         * Unlike modifications that affect the method's instructions (which uses toReplace etc),
         * we can directly modify the local variable table - it does not affect the frames of the
         * ProdCons analysis.
         */
        def updateLocalVariableTypes(reTypedLocals: Map[Int, Type]): Unit = {
          lazy val localsByIndex = method.localVariables.asScala.groupBy(_.index)
          for ((index, tp) <- reTypedLocals) localsByIndex.get(index).map(_.toList) match {
            case Some(List(local)) =>
              local.desc = tp.getDescriptor
            case Some(locals) =>
              locals foreach method.localVariables.remove
            case _ =>
          }
        }

        /* Remove box creations - leave the boxed value(s) on the stack instead. */
        def replaceCreationOps(): Unit = {
          for (creation <- allCreations) creation.loadInitialValues match {
            case None =>
              toDelete ++= creation.allInsns

            case Some(ops) =>
              toReplace(creation.valuesConsumer) = ops
              toDelete ++= (creation.allInsns - creation.valuesConsumer)
          }
        }

        /*
         * Replace a value extraction operation. For a single-value box, the extraction operation can
         * just be removed. An extraction from a multi-value box is replaced by POP operations for the
         * non-used values, and an xSTORE / xLOAD for the extracted value. Example: tuple3._2 becomes
         * POP; xSTORE n; POP; xLOAD n.
         */
        def replaceExtractionOps(): Unit = {
          if (boxKind.boxedTypes.lengthCompare(1) == 0) {
            // fast path for single-value boxes
            allConsumers.foreach(extraction => extraction.postExtractionAdaptationOps(boxKind.boxedTypes.head) match {
              case Nil => extraction match {
                case Drop(_) => toReplace(extraction.consumer) = boxKind.boxedTypes.map(t => getPop(t.getSize))
                case _ => toDelete ++= extraction.allInsns
              }
              case ops =>
                toReplace(extraction.consumer) = ops
                toDelete ++= extraction.allInsns - extraction.consumer
            })
          } else {
            for (extraction <- allConsumers) {
              val replacementOps = extraction match {
                case Drop(_) =>
                  boxKind.boxedTypes.reverseIterator.map(t => getPop(t.getSize)).toList
                case _ =>
                  val valueIndex = boxKind.extractedValueIndex(extraction)
                  if (valueIndex == 0) {
                    val pops = boxKind.boxedTypes.tail.map(t => getPop(t.getSize))
                    pops ::: extraction.postExtractionAdaptationOps(boxKind.boxedTypes.head)
                  } else {
                    var loadOps: List[AbstractInsnNode] = null
                val consumeStack = boxKind.boxedTypes.zipWithIndex.reverseIterator.map {
                      case (tp, i) =>
                        if (i == valueIndex) {
                          val resultSlot = getLocal(tp.getSize)
                          loadOps = new VarInsnNode(tp.getOpcode(ILOAD), resultSlot) :: extraction.postExtractionAdaptationOps(tp)
                          new VarInsnNode(tp.getOpcode(ISTORE), resultSlot)
                        } else {
                          getPop(tp.getSize)
                        }
                }.to(List)
                    consumeStack ::: loadOps
                  }
              }
              toReplace(extraction.consumer) = replacementOps
              toDelete ++= extraction.allInsns - extraction.consumer
            }
          }
        }

        checkCopyOpReplacements(allCreations, allConsumers, boxKind.boxedTypes, nextLocal, prodCons) match {
          case Some((replacements, nextCopyOpLocal, reTypedLocals)) =>
            toReplace ++= replacements
            updateLocalVariableTypes(reTypedLocals)
            nextLocal = nextCopyOpLocal
            replaceCreationOps()
            replaceExtractionOps()
            // Conservative (safe) value for stack growth. In every frame that initially has a multi-value
            // box on the stack, the stack now contains all of the individual values. So for every eliminated
            // box, the maxStack may be up to N-1 slots larger.
            maxStackGrowth += boxKind.boxedTypes.length - 1

          case None =>
        }
      }

      val it = method.instructions.iterator
      while (it.hasNext) {
        val insn = it.next()
        if (!knownHandled(insn)) BoxKind.valueCreationKind(insn, prodCons) match {
          case Some((boxCreation, boxKind)) =>
            allCreationsConsumers(boxCreation, boxKind, prodCons) match {
              case Some((allCreations, allConsumers)) =>
                val (escapingConsumers, boxConsumers) = allConsumers.partition(_.isEscaping)
                if (boxConsumers.nonEmpty) {
                  for (c <- allCreations) knownHandled ++= c.allInsns
                  for (e <- allConsumers) knownHandled ++= e.allInsns

                  val hasEscaping = escapingConsumers.nonEmpty
                  val hasWrite = allConsumers.exists(_.isWrite)
                  if (!hasEscaping && !hasWrite) {
                    // M2 -- see doc comment in the beginning of this file
                    // If both M1 and M2 can be applied, we prefer M2 because it doesn't introduce new locals.
                    replaceBoxOperationsMultipleCreations(allCreations, allConsumers, boxKind)
                  } else if (allCreations.size == 1 && (!hasEscaping || !boxKind.isMutable)) {
                    // M1 -- see doc comment in the beginning of this file
                    replaceBoxOperationsSingleCreation(allCreations.head, allConsumers, boxKind, keepBox = hasEscaping)
                  }
                }

              case None =>
            }

          case None =>
        }
      }

      // We don't need to worry about CallGraph.closureInstantiations and
      // BackendUtils.indyLambdaImplMethods, the removed instructions are not IndyLambdas
      def removeFromCallGraph(insn: AbstractInsnNode): Unit = insn match {
        case mi: MethodInsnNode => callGraph.removeCallsite(mi, method)
        case _ =>
      }

      for ((location, ops) <- toInsertBefore; op <- ops)
        method.instructions.insertBefore(location, op)

      for ((oldOp, newOps) <- toReplace) {
        for (newOp <- newOps) method.instructions.insertBefore(oldOp, newOp)
        method.instructions.remove(oldOp)
        removeFromCallGraph(oldOp)
      }

      for (op <- toDelete) {
        method.instructions.remove(op)
        removeFromCallGraph(op)
      }

      method.maxLocals = nextLocal
      method.maxStack = BackendUtils.maxStack(method) + maxStackGrowth
      val changed = toInsertBefore.nonEmpty || toReplace.nonEmpty || toDelete.nonEmpty
      changed
    }
  }

  /**
   * Given a box creations operation
   *   - find all ultimate consumers for the produced value. then:
   *     - for all consumed values, find all producer operations. check that all are box creations
   *       - recurse until reaching a fixpoint
   *
   * Returns a set of box creations and a set of box consumers. Note that the box consumers may
   * contain [[EscapingConsumer]]s, even if there are multiple box creation operations. The callee
   * will handle this case (and not attempt to eliminate the box).
   */
  def allCreationsConsumers(initialCreation: BoxCreation, boxKind: BoxKind, prodCons: ProdConsAnalyzer): Option[(Set[BoxCreation], Set[BoxConsumer])] = {
    var creations = Set(initialCreation)
    var consumers = Set.empty[BoxConsumer]

    def addCreations(boxConsumer: BoxConsumer): Boolean = {
      val newProds = boxConsumer.boxProducers(prodCons).filterNot(prod => creations.exists(_.producer == prod))
      newProds.forall(prod => boxKind.checkBoxCreation(prod, prodCons) match {
        case Some(boxCreation) =>
          creations += boxCreation
          addBoxConsumers(boxCreation)

        case _ => false
      })
    }

    def addBoxConsumers(creation: BoxCreation): Boolean = {
      val newCons = creation.boxConsumers(prodCons, ultimate = true).filterNot(cons => consumers.exists(_.consumer == cons))
      newCons.forall(cons => boxKind.checkBoxConsumer(cons, prodCons) match {
        case Some(boxConsumer) =>
          consumers += boxConsumer
          addCreations(boxConsumer)

        case _ =>
          creations.size <= 1 && {
            // If there's a single box creation, the box operations can still be rewritten
            consumers += EscapingConsumer(cons)
            true
          }
      })
    }

    if (addBoxConsumers(initialCreation)) Some((creations, consumers))
    else None
  }

  /**
   * Takes two sets `initialProds` and `finalCons` such that all boxes produced by the first set
   * are only consumed by an operation in the second set.
   *
   * Returns a map that replaces copy operations (ALOAD / ASTORE) between the producers and
   * consumers with corresponding copy operations for the values stored in the box. The returned
   * `Int` value returns the next free local variable slot.
   *
   * Examples:
   *   - for an Integer box, an ASTORE x is simply replaced by ISTORE x
   *   - for a pair of two references, an ASTORE x is replaced by `ASTORE x1; ASTORE x2` where x1
   *     and x2 are fresh locals
   *
   * Not all copy operations can be supported: DUP only works for single-value boxes, the more
   * exotic copy operations (DUP2_X2) are not supported (note that Scalac never emits them). If a
   * copy operation cannot be replaced, this method returns `None`.
   */
  def checkCopyOpReplacements(initialProds: Set[BoxCreation], finalCons: Set[BoxConsumer], valueTypes: List[Type], nextLocal: Int, prodCons: ProdConsAnalyzer): Option[(Map[AbstractInsnNode, List[AbstractInsnNode]], Int, Map[Int, Type])] = {
    var replacements = Map.empty[AbstractInsnNode, List[AbstractInsnNode]]
    var reTypedLocals = Map.empty[Int, Type]

    var nextCopyOpLocal = nextLocal
    val newLocalsMap: mutable.LongMap[List[(Type, Int)]] = mutable.LongMap.empty
    def newLocals(index: Int) = newLocalsMap.getOrElseUpdate(index, valueTypes match {
      case List(t) if t.getSize == 1 =>
        reTypedLocals += index -> t
        List((t, index))
      case _ => valueTypes.map(t => {
        val newIndex = nextCopyOpLocal
        nextCopyOpLocal += t.getSize
        (t, newIndex)
      })
    })

    var replaceOK = true
    val copyOps = new CopyOpsIterator(initialProds, finalCons, prodCons)
    while (replaceOK && copyOps.hasNext) copyOps.next() match {
      case vi: VarInsnNode =>
        val isLoad = vi.getOpcode == ALOAD
        val typedVarOp = (tp: (Type, Int)) => {
          val opc = tp._1.getOpcode(if (isLoad) ILOAD else ISTORE)
          new VarInsnNode(opc, tp._2)
        }
        val locs = newLocals(vi.`var`)
        replacements += vi -> (if (isLoad) locs.map(typedVarOp) else locs.map(typedVarOp).reverse)

      case copyOp =>
        if (copyOp.getOpcode == DUP && valueTypes.lengthCompare(1) == 0) {
          if (valueTypes.head.getSize == 2)
            replacements += copyOp -> List(new InsnNode(DUP2))
        } else {
          replaceOK = false
        }
    }
    if (replaceOK) Some((replacements, nextCopyOpLocal, reTypedLocals)) else None
  }

  /**
   * For a set of box creation operations and a corresponding set of box consumer operations,
   * this iterator returns all copy operations (load, store, dup) that are in between.
   */
  class CopyOpsIterator(initialCreations: Set[BoxCreation], finalCons: Set[BoxConsumer], prodCons: ProdConsAnalyzer) extends AbstractIterator[AbstractInsnNode] {
    private val queue = mutable.Queue.empty[AbstractInsnNode] ++= initialCreations.iterator.flatMap(_.boxConsumers(prodCons, ultimate = false))

    // a single copy operation can consume multiple producers: val a = if (b) box(1) else box(2).
    // the `ASTORE a` has two producers (the two box operations). we need to handle it only once.
    private val visited = mutable.Set.empty[AbstractInsnNode]

    private val boxConsumingOps = finalCons.map(_.consumer)

    @tailrec private def advanceToNextCopyOp(): Unit = {
      if (queue.nonEmpty) {
        val h = queue.front
        if (visited(h) || boxConsumingOps(h)) {
          queue.dequeue()
          advanceToNextCopyOp()
        }
      }
    }

    def hasNext: Boolean = {
      advanceToNextCopyOp()
      queue.nonEmpty
    }

    def next(): AbstractInsnNode = {
      advanceToNextCopyOp()
      val r = queue.dequeue()
      visited += r
      queue ++= prodCons.consumersOfOutputsFrom(r)
      r
    }
  }

  trait BoxKind {
    def checkBoxCreation(insn: AbstractInsnNode, prodCons: ProdConsAnalyzer): Option[BoxCreation]
    def checkBoxConsumer(insn: AbstractInsnNode, prodCons: ProdConsAnalyzer): Option[BoxConsumer]
    def boxedTypes: List[Type]
    def extractedValueIndex(extraction: BoxConsumer): Int
    def isMutable: Boolean
  }

  object BoxKind {
    def valueCreationKind(insn: AbstractInsnNode, prodCons: ProdConsAnalyzer): Option[(BoxCreation, BoxKind)] = {
      PrimitiveBox.checkPrimitiveBox(insn, None, prodCons) orElse
        Ref.checkRefCreation(insn, None, prodCons) orElse
        Tuple.checkTupleCreation(insn, None, prodCons)
    }

    /**
     * Check if `newOp` is part of a standard object construction pattern in which:
     *
     *   NEW T
     *   DUP
     *   [load constructor args]
     *   INVOKESPECIAL T.init
     *
     * The method ensures that the entire construction pattern is closed in itself, without any
     * branches going in or out. This is checked by looking at producers / consumers:
     *   - `DUP` is the only consumer of `NEW`, and vice versa
     *   - `DUP` the only producer for the receiver of the constructor call
     *   - The set of consumers of `DUP` without the constructor call is the same as
     *     the set of consumers of the value on the stack top after the constructor call
     */
    def checkInstanceCreation(newOp: TypeInsnNode, prodCons: ProdConsAnalyzer): Option[(InsnNode, MethodInsnNode)] = {
      val newCons = prodCons.consumersOfOutputsFrom(newOp)
      if (newCons.size == 1 && newCons.head.getOpcode == DUP) {
        val dupOp = newCons.head.asInstanceOf[InsnNode]
        if (prodCons.producersForInputsOf(dupOp) == Set(newOp)) {
          val dupCons = prodCons.consumersOfOutputsFrom(dupOp)
          val initCalls = dupCons collect {
            case mi: MethodInsnNode if mi.name == GenBCode.INSTANCE_CONSTRUCTOR_NAME && mi.owner == newOp.desc => mi
          }
          if (initCalls.size == 1) {
            val initCall = initCalls.head
            val numArgs = Type.getArgumentTypes(initCall.desc).length
            val receiverProds = prodCons.producersForValueAt(initCall, prodCons.frameAt(initCall).stackTop - numArgs)
            if (receiverProds == Set(dupOp)) {
              val dupConsWithoutInit = dupCons - initCall
              val afterInit = initCall.getNext
              val stackTopAfterInit = prodCons.frameAt(afterInit).stackTop
              val initializedInstanceCons = prodCons.consumersOfValueAt(afterInit, stackTopAfterInit)
              if (initializedInstanceCons == dupConsWithoutInit) {
                return Some((dupOp, initCall))
              }
            }
          }
        }
      }
      None
    }

    /**
     * If `mi` is an invocation of a method on Predef, check if the receiver is a GETSTATIC of
     * Predef.MODULE\$ and return it.
     */
    def checkReceiverPredefLoad(mi: MethodInsnNode, prodCons: ProdConsAnalyzer): Option[AbstractInsnNode] = {
      val numArgs = Type.getArgumentTypes(mi.desc).length
      val receiverProds = prodCons.producersForValueAt(mi, prodCons.frameAt(mi).stackTop - numArgs)
      if (receiverProds.size == 1) {
        val prod = receiverProds.head
        if (isPredefLoad(prod) && prodCons.consumersOfOutputsFrom(prod) == Set(mi)) return Some(prod)
      }
      None
    }
  }

  case class PrimitiveBox(boxedType: Type, boxClass: InternalName) extends BoxKind {
    import PrimitiveBox._
    def checkBoxCreation(insn: AbstractInsnNode, prodCons: ProdConsAnalyzer): Option[BoxCreation] = checkPrimitiveBox(insn, Some(this), prodCons).map(_._1)
    def checkBoxConsumer(insn: AbstractInsnNode, prodCons: ProdConsAnalyzer): Option[BoxConsumer] = checkPrimitiveUnbox(insn, this, prodCons)
    def boxedTypes: List[Type] = List(boxedType)
    def extractedValueIndex(extraction: BoxConsumer): Int = 0
    def isMutable = false
  }

  object PrimitiveBox {
    private def boxedType(mi: MethodInsnNode) = Type.getArgumentTypes(mi.desc)(0)

    private def boxClass(mi: MethodInsnNode) = {
      if (mi.name == GenBCode.INSTANCE_CONSTRUCTOR_NAME) mi.owner
      else Type.getReturnType(mi.desc).getInternalName
    }

    private val primBoxSupertypes: Map[InternalName, Set[InternalName]] = {
      import postProcessor.bTypes._
      def transitiveSupertypes(clsbt: ClassBType): Set[ClassBType] =
        (clsbt.info.get.superClass ++ clsbt.info.get.interfaces).flatMap(transitiveSupertypes).toSet + clsbt

      coreBTypes.boxedClasses.map { bc =>
        bc.internalName -> (transitiveSupertypes(bc).map(_.internalName) + bc.internalName)
      }.toMap
    }

    def checkPrimitiveBox(insn: AbstractInsnNode, expectedKind: Option[PrimitiveBox], prodCons: ProdConsAnalyzer): Option[(BoxCreation, PrimitiveBox)] = {
      // mi is either a box factory or a box constructor invocation
      def checkKind(mi: MethodInsnNode) = expectedKind match {
        case Some(kind) => if (kind.boxClass == boxClass(mi)) expectedKind else None
        case None => Some(PrimitiveBox(boxedType(mi), boxClass(mi)))
      }

      insn match {
        case mi: MethodInsnNode =>
          if (isScalaBox(mi) || isJavaBox(mi)) checkKind(mi).map((StaticFactory(mi, loadInitialValues = None), _))
          else if (isPredefAutoBox(mi))
            for (predefLoad <- BoxKind.checkReceiverPredefLoad(mi, prodCons); kind <- checkKind(mi))
              yield (ModuleFactory(predefLoad, mi), kind)
          else None

        case ti: TypeInsnNode if ti.getOpcode == NEW =>
          for ((dupOp, initCall) <- BoxKind.checkInstanceCreation(ti, prodCons) if isPrimitiveBoxConstructor(initCall); kind <- checkKind(initCall))
            yield (InstanceCreation(ti, dupOp, initCall), kind)

        case _ => None
      }
    }

    def checkPrimitiveUnbox(insn: AbstractInsnNode, kind: PrimitiveBox, prodCons: ProdConsAnalyzer): Option[BoxConsumer] = {
      def typeOK(mi: MethodInsnNode) = kind.boxedType == Type.getReturnType(mi.desc)
      insn match {
        case mi: MethodInsnNode =>
          if ((isScalaUnbox(mi) || isJavaUnbox(mi)) && typeOK(mi)) Some(StaticGetterOrInstanceRead(mi))
          else if (isPredefAutoUnbox(mi) && typeOK(mi)) BoxKind.checkReceiverPredefLoad(mi, prodCons).map(ModuleGetter(_, mi))
          else None

        case ti: TypeInsnNode if insn.getOpcode == INSTANCEOF =>
          val success = primBoxSupertypes(kind.boxClass).contains(ti.desc)
          Some(BoxedPrimitiveTypeCheck(ti, success))

        case i: InsnNode if i.getOpcode == POP =>
          Some(Drop(i))

        case _ => None
      }
    }
  }

  case class Ref(boxedType: Type, refClass: InternalName) extends BoxKind {
    import Ref._
    def checkBoxCreation(insn: AbstractInsnNode, prodCons: ProdConsAnalyzer): Option[BoxCreation] = checkRefCreation(insn, Some(this), prodCons).map(_._1)
    def checkBoxConsumer(insn: AbstractInsnNode, prodCons: ProdConsAnalyzer): Option[BoxConsumer] = checkRefConsumer(insn, this, prodCons)
    def boxedTypes: List[Type] = List(boxedType)
    def extractedValueIndex(extraction: BoxConsumer): Int = 0
    def isMutable = true
  }

  object Ref {
    private def boxedType(mi: MethodInsnNode): Type = runtimeRefClassBoxedType(mi.owner)
    private def refClass(mi: MethodInsnNode): InternalName = mi.owner
    private def loadZeroValue(refZeroCall: MethodInsnNode): List[AbstractInsnNode] = List(loadZeroForTypeSort(runtimeRefClassBoxedType(refZeroCall.owner).getSort))

    private val refSupertypes = {
      import postProcessor.bTypes._
      Set(coreBTypes.jiSerializableRef, coreBTypes.ObjectRef).map(_.internalName)
    }

    def checkRefCreation(insn: AbstractInsnNode, expectedKind: Option[Ref], prodCons: ProdConsAnalyzer): Option[(BoxCreation, Ref)] = {
      def checkKind(mi: MethodInsnNode): Option[Ref] = expectedKind match {
        case Some(kind) => if (kind.refClass == refClass(mi)) expectedKind else None
        case None => Some(Ref(boxedType(mi), refClass(mi)))
      }

      insn match {
        case mi: MethodInsnNode =>
          if (isRefCreate(mi)) checkKind(mi).map((StaticFactory(mi, loadInitialValues = None), _))
          else if (isRefZero(mi)) checkKind(mi).map((StaticFactory(mi, loadInitialValues = Some(loadZeroValue(mi))), _))
          else None

        case ti: TypeInsnNode if ti.getOpcode == NEW =>
          for ((dupOp, initCall) <- BoxKind.checkInstanceCreation(ti, prodCons) if isRuntimeRefConstructor(initCall); kind <- checkKind(initCall))
            yield (InstanceCreation(ti, dupOp, initCall), kind)

        case _ => None
      }
    }

    def checkRefConsumer(insn: AbstractInsnNode, kind: Ref, @unused prodCons: ProdConsAnalyzer): Option[BoxConsumer] = insn match {
      case fi: FieldInsnNode if fi.owner == kind.refClass && fi.name == "elem" =>
        if (fi.getOpcode == GETFIELD) Some(StaticGetterOrInstanceRead(fi))
        else if (fi.getOpcode == PUTFIELD) Some(StaticSetterOrInstanceWrite(fi))
        else None

      case ti: TypeInsnNode if ti.getOpcode == INSTANCEOF =>
        Some(BoxedPrimitiveTypeCheck(ti, ti.desc == kind.refClass || refSupertypes.contains(ti.desc)))

      case i: InsnNode if i.getOpcode == POP =>
        Some(Drop(i))

      case _ => None
    }
  }

  case class Tuple(boxedTypes: List[Type], tupleClass: InternalName) extends BoxKind {
    import Tuple._
    def checkBoxCreation(insn: AbstractInsnNode, prodCons: ProdConsAnalyzer): Option[BoxCreation] = checkTupleCreation(insn, Some(this), prodCons).map(_._1)
    def checkBoxConsumer(insn: AbstractInsnNode, prodCons: ProdConsAnalyzer): Option[BoxConsumer] = checkTupleExtraction(insn, this, prodCons)
    def extractedValueIndex(extraction: BoxConsumer): Int = extraction match {
      case StaticGetterOrInstanceRead(mi: MethodInsnNode) => tupleGetterIndex(mi.name)
      case PrimitiveBoxingGetter(mi)                      => tupleGetterIndex(mi.name)
      case PrimitiveUnboxingGetter(mi, _)                 => tupleGetterIndex(mi.name)
      case _ => throw new AssertionError(s"Expected tuple getter, found $extraction")
    }
    def isMutable = false
  }

  object Tuple {
    private def boxedTypes(mi: MethodInsnNode): List[Type] = Type.getArgumentTypes(mi.desc).toList
    private def tupleClass(mi: MethodInsnNode): InternalName = mi.owner

    def checkTupleCreation(insn: AbstractInsnNode, expectedKind: Option[Tuple], prodCons: ProdConsAnalyzer): Option[(BoxCreation, Tuple)] = {
      def checkKind(mi: MethodInsnNode): Option[Tuple] = expectedKind match {
        case Some(kind) => if (kind.tupleClass == tupleClass(mi)) expectedKind else None
        case None => Some(Tuple(boxedTypes(mi), tupleClass(mi)))
      }

      insn match {
        // no need to check for TupleN.apply: the compiler transforms case companion apply calls to constructor invocations
        case ti: TypeInsnNode if ti.getOpcode == NEW =>
          for ((dupOp, initCall) <- BoxKind.checkInstanceCreation(ti, prodCons) if isTupleConstructor(initCall); kind <- checkKind(initCall))
            yield (InstanceCreation(ti, dupOp, initCall), kind)

        case _ => None
      }
    }

    private val specializedTupleClassR = "scala/Tuple[12]\\$mc[IJDCZ]{1,2}\\$sp".r
    private def isSpecializedTupleClass(tupleClass: InternalName) = specializedTupleClassR.pattern.matcher(tupleClass).matches

    private val specializedTupleGetterR = "_[12]\\$mc[IJDCZ]\\$sp".r
    private def isSpecializedTupleGetter(mi: MethodInsnNode) = specializedTupleGetterR.pattern.matcher(mi.name).matches

    private val tupleGetterR = "_\\d\\d?".r
    private def isTupleGetter(mi: MethodInsnNode) = tupleGetterR.pattern.matcher(mi.name).matches

    def checkTupleExtraction(insn: AbstractInsnNode, kind: Tuple, prodCons: ProdConsAnalyzer): Option[BoxConsumer] = {
      val expectedTupleClass = kind.tupleClass
      insn match {
        case mi: MethodInsnNode =>
          val tupleClass = mi.owner
          if (isSpecializedTupleClass(expectedTupleClass)) {
            val typeOK = tupleClass == expectedTupleClass || tupleClass == expectedTupleClass.substring(0, expectedTupleClass.indexOf('$'))
            if (typeOK) {
              if (isSpecializedTupleGetter(mi)) return Some(StaticGetterOrInstanceRead(mi))
              else if (isTupleGetter(mi)) return Some(PrimitiveBoxingGetter(mi))
            }
          } else if (expectedTupleClass == tupleClass) {
            if (isSpecializedTupleGetter(mi)) return Some(PrimitiveUnboxingGetter(mi, Type.getReturnType(mi.desc)))
            else if (isTupleGetter(mi)) return Some(StaticGetterOrInstanceRead(mi))
          }

        case _ =>
          if (insn.getOpcode == POP) return Some(Drop(insn))
      }
      None
    }

    private val getterIndexPattern = "_(\\d{1,2}).*".r
    def tupleGetterIndex(getterName: String) = getterName match { case getterIndexPattern(i) => i.toInt - 1 case x => throw new MatchError(x) }
  }

  // TODO: add more
  // case class ValueClass(valueClass: Type, valueType: Type) extends BoxKind

  sealed trait BoxCreation {
    // to support box creation operations that don't consume an initial value from the stack, e.g., IntRef.zero
    val loadInitialValues: Option[List[AbstractInsnNode]]

    /**
     * The instruction that produces the box value; for instance creations, the `NEW` operation.
     */
    def producer: AbstractInsnNode

    /**
     * The instruction that consumes the boxed values; for instance creations, the `init` call.
     */
    def valuesConsumer: MethodInsnNode = this match {
      case StaticFactory(call, _) => call
      case ModuleFactory(_, call) => call
      case InstanceCreation(_, _, initCall) => initCall
    }

    def allInsns: Set[AbstractInsnNode] = this match {
      case StaticFactory(c, _) => Set(c)
      case ModuleFactory(m, c) => Set(m, c)
      case InstanceCreation(n, d, i) => Set(n, d, i)
    }

    /**
     * The consumers of the box produced by this box creation. If `ultimate` is true, then the
     * final consumers are returned (e.g., an unbox operation), otherwise direct consumers (e.g.,
     * a store operation).
     */
    def boxConsumers(prodCons: ProdConsAnalyzer, ultimate: Boolean): Set[AbstractInsnNode] = {
      val startInsn = this match {
        // for the non-transitive case (ultimate == false), it's important to start at the `dupOp`,
        // not the `newOp` - look at the BoxCreation as a black box, get its consumers.
        case InstanceCreation(_, dupOp, _) => dupOp
        case _ => producer
      }
      val cons = if (ultimate) prodCons.ultimateConsumersOfOutputsFrom(startInsn) else prodCons.consumersOfOutputsFrom(startInsn)
      this match {
        case InstanceCreation(_, _, initCall) => cons - initCall
        case _ => cons
      }
    }
  }

  case class StaticFactory(producer: MethodInsnNode, loadInitialValues: Option[List[AbstractInsnNode]]) extends BoxCreation
  case class ModuleFactory(moduleLoad: AbstractInsnNode, producer: MethodInsnNode) extends BoxCreation {
    val loadInitialValues: Option[List[AbstractInsnNode]] = None
  }
  case class InstanceCreation(newOp: TypeInsnNode, dupOp: InsnNode, initCall: MethodInsnNode) extends BoxCreation {
    def producer = newOp
    val loadInitialValues: Option[List[AbstractInsnNode]] = None
  }

  sealed trait BoxConsumer {
    val consumer: AbstractInsnNode

    def allInsns: Set[AbstractInsnNode] = this match {
      case ModuleGetter(m, c) => Set(m, c)
      case _ => Set(consumer)
    }

    /**
     * The initial producers of the box value consumed by this box consumer
     */
    def boxProducers(prodCons: ProdConsAnalyzer): Set[AbstractInsnNode] = {
      val stackTop = prodCons.frameAt(consumer).stackTop
      val slot = if (isWrite) stackTop - 1 else stackTop
      prodCons.initialProducersForValueAt(consumer, slot)
    }

    def isEscaping = this match {
      case _: EscapingConsumer => true
      case _ => false
    }

    def isWrite = this match {
      case _: StaticSetterOrInstanceWrite => true
      case _ => false
    }

    /**
     * If this box consumer extracts a boxed value and applies a conversion, this method returns
     * equivalent conversion operations. For example, invoking `_1\$mcI\$sp` on a non-specialized
     * `Tuple2` extracts the Integer value and unboxes it.
     */
    def postExtractionAdaptationOps(typeOfExtractedValue: Type): List[AbstractInsnNode] = this match {
      case PrimitiveBoxingGetter(_) => List(getScalaBox(typeOfExtractedValue))
      case PrimitiveUnboxingGetter(_, unboxedPrimitive) => List(getScalaUnbox(unboxedPrimitive))
      case BoxedPrimitiveTypeCheck(_, success) =>
        getPop(typeOfExtractedValue.getSize) ::
          new InsnNode(if (success) ICONST_1 else ICONST_0) ::
          Nil
      case _ => Nil
    }
  }

  /** Static extractor (BoxesRunTime.unboxToInt) or GETFIELD or getter invocation */
  case class StaticGetterOrInstanceRead(consumer: AbstractInsnNode) extends BoxConsumer
  /** A getter that boxes the returned value, e.g., `Tuple2\$mcII\$sp._1` */
  case class PrimitiveBoxingGetter(consumer: MethodInsnNode) extends BoxConsumer
  /** A getter that unboxes the returned value, e.g., `Tuple2._1\$mcI\$sp` */
  case class PrimitiveUnboxingGetter(consumer: MethodInsnNode, unboxedPrimitive: Type) extends BoxConsumer
  /** An extractor method in a Scala module, e.g., `Predef.Integer2int` */
  case class ModuleGetter(moduleLoad: AbstractInsnNode, consumer: MethodInsnNode) extends BoxConsumer
  /** PUTFIELD or setter invocation */
  case class StaticSetterOrInstanceWrite(consumer: AbstractInsnNode) extends BoxConsumer
  /** `.\$isInstanceOf[T]` (can be statically proven true or false) */
  case class BoxedPrimitiveTypeCheck(consumer: AbstractInsnNode, success: Boolean) extends BoxConsumer
  /** POP */
  case class Drop(consumer: AbstractInsnNode) extends BoxConsumer
  /** An unknown box consumer */
  case class EscapingConsumer(consumer: AbstractInsnNode) extends BoxConsumer
}
