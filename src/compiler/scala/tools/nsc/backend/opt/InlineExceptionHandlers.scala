/* NSC -- new scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 */

package scala.tools.nsc
package backend.opt

import java.util.concurrent.TimeUnit

/**
  * This optimization phase inlines the exception handlers so that further phases can optimize the code better
  *
  * {{{
  * try {
  *   ...
  *   if (condition)
  *     throw IllegalArgumentException("sth")
  * } catch {
  *   case e: IllegalArgumentException => <handler code>
  *   case e: ... => ...
  * }
  * }}}
  *
  * will inline the exception handler code to:
  *
  * {{{
  * try {
  *   ...
  *   if (condition)
  *     <handler code> // + jump to the end of the catch statement
  * } catch {
  *   case e: IllegalArgumentException => <handler code>
  *   case e: ... => ...
  * }
  * }}}
  *
  * Q: How does the inlining work, ICode level?
  * A: if a block contains a THROW(A) instruction AND there is a handler that takes A or a superclass of A we do:
  *    1. We duplicate the handler code such that we can transform THROW into a JUMP
  *    2. We analyze the handler to see what local it expects the exception to be placed in
  *    3. We place the exception that is thrown in the correct "local variable" slot and clean up the stack
  *    4. We finally JUMP to the duplicate handler
 *    All the above logic is implemented in InlineExceptionHandlersPhase.apply(bblock: BasicBlock)
  *
  * Q: Why do we need to duplicate the handler?
  * A: An exception might be thrown in a method that we invoke in the function and we cannot see that THROW command
  * directly. In order to catch such exceptions, we keep the exception handler in place and duplicate it in order
  * to inline its code.
  *
  * @author Vlad Ureche
  */
abstract class InlineExceptionHandlers extends SubComponent {
  import global._
  import icodes._
  import icodes.opcodes._

  val phaseName = "inlinehandlers"

  /** Create a new phase */
  override def newPhase(p: Phase) = new InlineExceptionHandlersPhase(p)

  override def enabled = settings.inlineHandlers

  /**
    * Inlining Exception Handlers
    */
  class InlineExceptionHandlersPhase(prev: Phase) extends ICodePhase(prev) {
    def name = phaseName

    /* This map is used to keep track of duplicated exception handlers
     * explanation: for each exception handler basic block, there is a copy of it
     * -some exception handler basic blocks might not be duplicated because they have an unknown format => Option[(...)]
     * -some exception handler duplicates expect the exception on the stack while others expect it in a local
     *   => Option[Local]
     */
    private val handlerCopies = perRunCaches.newMap[BasicBlock, Option[(Option[Local], BasicBlock)]]()
    /* This map is the inverse of handlerCopies, used to compute the stack of duplicate blocks */
    private val handlerCopiesInverted = perRunCaches.newMap[BasicBlock, (BasicBlock, TypeKind)]()
    private def handlerLocal(bb: BasicBlock): Option[Local] =
      for (v <- handlerCopies get bb ; (local, block) <- v ; l <- local) yield l

    /* Type Flow Analysis */
    private val tfa: analysis.MethodTFA = new analysis.MethodTFA()
    private var tfaCache: Map[Int, tfa.lattice.Elem] = Map.empty
    private var analyzedMethod: IMethod = NoIMethod

    /* Blocks that need to be analyzed */
    private var todoBlocks: List[BasicBlock] = Nil

    /* Used only for warnings */
    private var currentClass: IClass = null

    /** Apply exception handler inlining to a class */
    override def apply(c: IClass): Unit =
      if (settings.inlineHandlers) {
        val startTime = System.nanoTime()
        currentClass = c

        debuglog("Starting InlineExceptionHandlers on " + c)
        c.methods foreach applyMethod
        debuglog("Finished InlineExceptionHandlers on " + c + "... " + TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - startTime) + "ms")
        currentClass = null
      }

    /**
      * Apply exception handler inlining to a method
      *
      * Note: for each exception handling block, we (might) create duplicates. Therefore we iterate until we get to a
      * fixed point where all the possible handlers have been inlined.
      *
      * TODO: Should we have an inlining depth limit? A nested sequence of n try-catch blocks can lead to at most 2n
      * inlined blocks, so worst case scenario we double the size of the code
      */
    private def applyMethod(method: IMethod): Unit = {
      if (method.hasCode) {
        // create the list of starting blocks
        todoBlocks = global.icodes.linearizer.linearize(method)

        while (todoBlocks.nonEmpty) {
          val levelBlocks = todoBlocks
          todoBlocks = Nil
          levelBlocks foreach applyBasicBlock // new blocks will be added to todoBlocks
        }
      }

      // Cleanup the references after we finished the file
      handlerCopies.clear()
      handlerCopiesInverted.clear()
      todoBlocks = Nil

      // Type flow analysis cleanup
      analyzedMethod = NoIMethod
      tfaCache = Map.empty
      //TODO: Need a way to clear tfa structures
    }

    /** Apply exception handler inlining to a basic block  */
    private def applyBasicBlock(bblock: BasicBlock): Unit = {
      /*
       * The logic of this entire method:
       *  - for each basic block, we look at each instruction until we find a THROW instruction
       *  - once we found a THROW instruction, we decide if it is DECIDABLE which of handler will catch the exception
       *  (see method findExceptionHandler for more details)
       *  - if we decided there is a handler that will catch the exception, we need to replace the THROW instruction by
       *  a set of equivalent instructions:
       *    * we need to compute the static types of the stack slots
       *    * we need to clear the stack, everything but the exception instance on top (or in a local variable slot)
       *    * we need to JUMP to the duplicate exception handler
       *  - we compute the static types of the stack slots in function getTypesAtInstruction
       *  - we duplicate the exception handler (and we get back the information of whether the duplicate expects the
       *  exception instance on top of the stack or in a local variable slot)
       *  - we compute the necessary code to put the exception in its place, clear the stack and JUMP
       *  - we change the THROW exception to the new Clear stack + JUMP code
       */
      for {
        (instr @ THROW(clazz), index) <- bblock.iterator.zipWithIndex
        // Decide if any handler fits this exception
        // If not, then nothing to do, we cannot determine statically which handler will catch the exception
        (handler, caughtException)    <- findExceptionHandler(toTypeKind(clazz.tpe), bblock.exceptionSuccessors)
      } {
        log("   Replacing " + instr + " in " + bblock + " to new handler")

        // Solve the stack and drop the element that we already stored, which should be the exception
        // needs to be done here to be the first thing before code becomes altered
        val typeInfo = getTypesAtInstruction(bblock, index)

        // Duplicate exception handler
        duplicateExceptionHandlerCache(handler) match {
          case None =>
            log("   Could not duplicate handler for " + instr + " in " + bblock)

          case Some((exceptionLocalOpt, newHandler)) =>
            val onStackException = typeInfo.head
            val thrownException  = toTypeKind(clazz.tpe)

            // A couple of sanity checks, to make sure we don't touch code we can't safely handle
            val canReplaceHandler = (
                  typeInfo.nonEmpty
              && (index == bblock.length - 1)
              && (onStackException <:< thrownException)
            )
            // in other words: what's on the stack MUST conform to what's in the THROW(..)!

            if (!canReplaceHandler) {
              reporter.warning(NoPosition, "Unable to inline the exception handler inside incorrect" +
                " block:\n" + bblock.iterator.mkString("\n") + "\nwith stack: " + typeInfo + " just " +
                "before instruction index " + index)
            }
            else {
              // Prepare the new code to replace the THROW instruction
              val newCode = exceptionLocalOpt match {
                // the handler duplicate expects the exception in a local: easy one :)
                case Some(local) =>
                  // in the first cycle we remove the exception Type
                  STORE_LOCAL(local) +: typeInfo.tail.map(x => DROP(x)) :+ JUMP(newHandler)

                // we already have the exception on top of the stack, only need to JUMP
                case None if typeInfo.length == 1 =>
                  JUMP(newHandler) :: Nil

                // we have the exception on top of the stack but we have other stuff on the stack
                // create a local, load exception, clear the stack and finally store the exception on the stack
                case _ =>
                  val exceptionType = typeInfo.head
                  // Here we could create a single local for all exceptions of a certain type. TODO: try that.
                  val localName   = currentClass.cunit.freshTermName("exception$")
                  val localType   = exceptionType
                  val localSymbol = bblock.method.symbol.newValue(localName).setInfo(localType.toType)
                  val local       = new Local(localSymbol, localType, false)

                  bblock.method.addLocal(local)

                  // Save the exception, drop the stack and place back the exception
                  STORE_LOCAL(local) :: typeInfo.tail.map(x => DROP(x)) ::: List(LOAD_LOCAL(local), JUMP(newHandler))
              }
              // replace THROW by the new code
              bblock.replaceInstruction(instr, newCode)

              // notify the successors changed for the current block
              // notify the predecessors changed for the inlined handler block
              bblock.touched = true
              newHandler.touched = true

              log("   Replaced  " + instr + " in " + bblock + " to new handler")
              log("OPTIMIZED class " + currentClass + " method " +
                bblock.method + " block " + bblock + " newhandler " +
                newHandler + ":\n\t\t" + onStackException + " <:< " +
                thrownException + " <:< " + caughtException)

          }
        }
      }
    }

    /**
      * Gets the types on the stack at a certain point in the program. Note that we want to analyze the method lazily
      * and therefore use the analyzedMethod variable
      */
    private def getTypesAtInstruction(bblock: BasicBlock, index: Int): List[TypeKind] = {
      // get the stack at the block entry
      var typeInfo = getTypesAtBlockEntry(bblock)

      // perform tfa to the current instruction
      log("         stack at the beginning of block " + bblock + " in function " +
        bblock.method + ": " + typeInfo.stack)
      for (i <- 0 to (index - 1)) {
        typeInfo = tfa.interpret(typeInfo, bblock(i))
        log("         stack after interpret: " + typeInfo.stack + " after instruction " +
          bblock(i))
      }
      log("         stack before instruction " + index + " of block " + bblock + " in function " +
        bblock.method + ": " + typeInfo.stack)

      // return the result
      typeInfo.stack.types
    }

    /**
      * Gets the stack at the block entry. Normally the typeFlowAnalysis should be run again, but we know how to compute
      * the stack for handler duplicates. For the locals, it's safe to assume the info from the original handler is
      * still valid (a more precise analysis can be done, but it's not necessary)
      */
    private def getTypesAtBlockEntry(bblock: BasicBlock): tfa.lattice.Elem = {
      // lazily perform tfa, because it's expensive
      // cache results by block label, as rewriting the code messes up the block's hashCode
      if (analyzedMethod eq NoIMethod) {
        analyzedMethod = bblock.method
        tfa.init(bblock.method)
        tfa.run()
        log("      performed tfa on method: " + bblock.method)

        for (block <- bblock.method.blocks.sortBy(_.label))
          tfaCache += block.label -> tfa.in(block)
      }

      log("         getting typeinfo at the beginning of block " + bblock)

      tfaCache.getOrElse(bblock.label, {
        // this block was not analyzed, but it's a copy of some other block so its stack should be the same
        log("         getting typeinfo at the beginning of block " + bblock + " as a copy of " +
          handlerCopiesInverted(bblock))
        val (origBlock, exception) = handlerCopiesInverted(bblock)
        val typeInfo               = getTypesAtBlockEntry(origBlock)
        val stack                  =
          if (handlerLocal(origBlock).nonEmpty) Nil   // empty stack, the handler copy expects an empty stack
          else List(exception)                        // one slot on the stack for the exception

        // If we use the mutability property, it crashes the analysis
        tfa.lattice.IState(new analysis.VarBinding(typeInfo.vars), new icodes.TypeStack(stack))
      })
    }

    /**
      * Finds the first exception handler that matches the current exception
      *
      * Note the following code:
      * {{{
      * try {
      *   throw new IllegalArgumentException("...")
      * } catch {
      *   case e: RuntimeException => log("RuntimeException")
      *   case i: IllegalArgumentException => log("IllegalArgumentException")
      * }
      * }}}
      *
      * will print "RuntimeException" => we need the *first* valid handler
      *
      * There's a hidden catch here: say we have the following code:
      * {{{
      * try {
      *   val exception: Throwable =
      *     if (scala.util.Random.nextInt % 2 == 0)
      *       new IllegalArgumentException("even")
      *     else
      *       new StackOverflowError("odd")
      *   throw exception
      * } catch {
      *   case e: IllegalArgumentException =>
      *     println("Correct, IllegalArgumentException")
      *   case e: StackOverflowError =>
      *     println("Correct, StackOverflowException")
      *   case t: Throwable =>
      *     println("WROOOONG, not Throwable!")
      * }
      * }}}
      *
      * We don't want to select a handler if there's at least one that's more specific!
      */
    def findExceptionHandler(thrownException: TypeKind, handlers: List[BasicBlock]): Option[(BasicBlock, TypeKind)] = {
      for (handler <- handlers ; LOAD_EXCEPTION(clazz) <- handler take 1) {
        val caughtException = toTypeKind(clazz.tpe)
        // we'll do inlining here: createdException <:< thrownException <:< caughtException, good!
        if (thrownException <:< caughtException)
          return Some((handler, caughtException))
        // we can't do inlining here, the handling mechanism is more precise than we can reason about
        if (caughtException <:< thrownException)
          return None
        // no result yet, look deeper in the handler stack
      }
      None
    }

    /**
      * This function takes care of duplicating the basic block code for inlining the handler
      *
      * Note: This function does not duplicate the same basic block twice. It will contain a map of the duplicated
      * basic blocks
      */
    private def duplicateExceptionHandlerCache(handler: BasicBlock) =
      handlerCopies.getOrElseUpdate(handler, duplicateExceptionHandler(handler))

    /** This function takes care of actual duplication */
    private def duplicateExceptionHandler(handler: BasicBlock): Option[(Option[Local], BasicBlock)] = {
      log("      duplicating handler block " + handler)

      handler take 2 match {
        case Seq(LOAD_EXCEPTION(caughtClass), next) =>
          val (dropCount, exceptionLocal) = next match {
            case STORE_LOCAL(local) => (2, Some(local)) // we drop both LOAD_EXCEPTION and STORE_LOCAL
            case _                  => (1, None)        // we only drop the LOAD_EXCEPTION and expect the exception on the stack
          }
          val caughtException = toTypeKind(caughtClass.tpe)
          // copy the exception handler code once again, dropping the LOAD_EXCEPTION
          val copy = handler.code.newBlock()
          copy.emitOnly((handler.iterator drop dropCount).toSeq: _*)

          // extend the handlers of the handler to the copy
          for (parentHandler <- handler.method.exh ; if parentHandler covers handler) {
            parentHandler.addCoveredBlock(copy)
            // notify the parent handler that the successors changed
            parentHandler.startBlock.touched = true
          }

          // notify the successors of the inlined handler might have changed
          copy.touched    = true
          handler.touched = true
          log("      duplicated  handler block " + handler + " to " + copy)

          // announce the duplicate handler
          handlerCopiesInverted(copy) = ((handler, caughtException))
          todoBlocks ::= copy

          Some((exceptionLocal, copy))

        case _ =>
          reporter.warning(NoPosition, "Unable to inline the exception handler due to incorrect format:\n" +
            handler.iterator.mkString("\n"))
          None
      }
    }
  }
}
