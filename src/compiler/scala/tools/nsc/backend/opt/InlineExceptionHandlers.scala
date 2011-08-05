/* NSC -- new scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 */

package scala.tools.nsc
package backend.opt
import scala.collection.immutable

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

  val phaseName = "inlineExceptionHandlers"

  /** Create a new phase */
  override def newPhase(p: Phase) = new InlineExceptionHandlersPhase(p)

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
    var handlerCopies: Map[BasicBlock, Option[(Option[Local], BasicBlock)]] = Map.empty
    var handlerCopiesInverted: Map[BasicBlock, BasicBlock] = Map.empty

    /* Type Flow Analysis */
    val tfa: analysis.MethodTFA = new analysis.MethodTFA()
    var analyzedMethod: IMethod = null

    /* This set stores the blocks that were analyzed and have no more inlining opportunities */
    var todoBlocks = Set.empty[BasicBlock]
    var doneBlocks = Set.empty[BasicBlock]

    /* Used only for warnings */
    var currentClass: IClass = null


    /** Apply exception handler inlining to a class */
    override def apply(c: IClass): Unit =
      if (settings.inline.value) { // should we be piggybacking the inliner setting?
        val startTime = System.currentTimeMillis
        currentClass = c

        log("Starting " + c.toString)
        for (method <- c.methods)
          apply(method)

        log("Finished " + c.toString + "... " + (System.currentTimeMillis - startTime) + "ms")
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
    def apply(method: IMethod): Unit = {

      if (method.code ne null) {
        // empty the list of copy blocks
        doneBlocks = Set.empty[BasicBlock]

        // create the list of starting blocks
        todoBlocks = method.code.blocks.filter(_.predecessors.length > 0).toSet + method.code.startBlock

        while (todoBlocks.size > doneBlocks.size) { // XXX: Should we set a threshhold for the inlining depth/count?
          val levelBlocks = todoBlocks -- doneBlocks
          for (bblock <- levelBlocks)
            apply(bblock) // new blocks will be added to todoBlocks
        }
      }

      // Cleanup the references after we finished the file
      handlerCopies = Map.empty
      handlerCopiesInverted = Map.empty
      todoBlocks = Set.empty
      doneBlocks = Set.empty
      //TODO: Need a way to clear tfa structures
    }

    /** Apply exception handler inlining to a basic block  */
    def apply(bblock: BasicBlock): Unit = {
      var index = 0
      doneBlocks = doneBlocks + bblock

      for (instr <- bblock) {
        instr match {
          case THROW(clazz) =>
            // Decide if any handler fits this exception
            findExceptionHandler(toTypeKind(clazz.tpe), bblock.exceptionSuccessors) match {
              case Some(handler) =>
                log("   Replacing " + instr.toString + " in " + bblock.toString + " to new handler")

                // Duplicate exception handler
                duplicateExceptionHandlerWithCaching(handler) match {

                  case Some((exceptionLocalOpt, newHandler)) =>
                    // Solve the stack and drop the element that we already stored, which should be the exception
                    val typeInfo = getTypesAtInstruction(bblock, index)
                    var canReplaceHandler = true

                    // Another batch of sanity checks
                    if (typeInfo.stack.length < 1)                        canReplaceHandler = false
                    if (index != bblock.length - 1)                       canReplaceHandler = false
                    //if (!(toTypeKind(clazz.tpe) <:< typeInfo.stack.head)) canReplaceHandler = false
                    /* This subtyping relation seems like a decent assumption. Unfortunately this doesn't always hold,
                     * if the inliner phase is enabled:
                     *
                     *   warning: Unable to inline the exception handler inside incorrect block:
                     *    CALL_PRIMITIVE(StringConcat(REF(class Object)))
                     *    CONSTANT("; see the documenter error output for details.")
                     *    CALL_PRIMITIVE(StringConcat(REF(class String)))
                     *    CALL_PRIMITIVE(EndConcat)
                     *    CALL_METHOD scala.tools.ant.ScalaTask.buildError (dynamic)
                     *    THROW(Throwable)
                     *   with stack: [REF(trait Nothing)] just before instruction index 5
                     */

                    if (!canReplaceHandler) {

                      assert(currentClass ne null)
                      currentClass.cunit.warning(NoPosition, "Unable to inline the exception handler inside incorrect" +
                        " block:\n" + bblock.mkString("\n") + "\nwith stack: " + typeInfo.stack.toString + " just " +
                        "before instruction index " + index)

                    } else {

                      var newCode: List[Instruction] = Nil

                      // Prepare the new code, depending on the local
                      exceptionLocalOpt match {

                        // we already know where to load the exception
                        case Some(exceptionLocal) =>
                          val exceptionType = typeInfo.stack.pop

                          newCode = STORE_LOCAL(exceptionLocal) :: Nil
                          while (typeInfo.stack.length != 0)
                            newCode = DROP(typeInfo.stack.pop) :: newCode
                          newCode = JUMP(newHandler) :: newCode

                        // we only have the exception on the stack, no need to do anything
                        case None if (typeInfo.stack.length == 1) =>
                          newCode = JUMP(newHandler) :: Nil

                        // we have the exception on top of the stack, create a local, load exception, clear the stack
                        // and finally store the exception on the stack
                        case None =>
                          val exceptionType = typeInfo.stack.pop

                          assert(currentClass ne null)
                          val localName = currentClass.cunit.freshTermName("exception$")
                          val localSymbol = bblock.method.symbol.newValue(localName)
                          val localType = exceptionType
                          val local = new Local(localSymbol, localType, false)
                          bblock.method.addLocal(local)

                          /* To pass the ICodeCheckers phase this has to be split to a new block since BasicBlock's
                           * indirectExceptionSuccessors introduce false successors that break the stack discipline
                           * at the beginning of the exception handler
                           * TODO: Check what is better: to have this complex code run or to have the exception thrown?
                           * TODO: Do we really care about ICodeCheckers? Tiark suggests ICodeCheckers might be too
                           * conservative (exception handler blocks should not have a meet operation between
                           * predecessors as they drop the stack anyway)
                           */
                          val bridgeBlock = bblock.code.newBlock
                          // no exception handlers over bridgeblock, there's no exception that could be thrown
                          bridgeBlock.emit(STORE_LOCAL(local), instr.pos)
                          while (typeInfo.stack.length != 0)
                            bridgeBlock.emit(DROP(typeInfo.stack.pop), instr.pos)
                          bridgeBlock.emit(LOAD_LOCAL(local), instr.pos)
                          bridgeBlock.emit(JUMP(newHandler), instr.pos)
                          bridgeBlock.close

                          newCode = JUMP(bridgeBlock) :: Nil
                      }
                      bblock.replaceInstruction(instr, newCode.reverse)
                      // notify the successors changed for the current block
                      // notify the predecessors changed for the inlined handler block
                      bblock.touched = true
                      newHandler.touched = true
                      log("   Replaced  " + instr.toString + " in " + bblock.toString + " to new handler")
                    }

                  case None =>
                    log("   Could not duplicate handler for " + instr.toString + " in " + bblock.toString)
                }
              case None =>
                // nothing to do, the throw will be caught outside the current method
            }
          case _ =>
        }
        index += 1
      }
    }

    /**
      * Gets the types on the stack at a certain point in the program. Note that we want to analyze the method lazily
      * and therefore use the analyzedMethod variable
      */
    def getTypesAtInstruction(bblock: BasicBlock, index: Int): tfa.lattice.Elem = {

      // lazily perform tfa, because it's expensive
      if ((analyzedMethod eq null) || (analyzedMethod != bblock.method)) {
        tfa.init(bblock.method)
        tfa.run
        analyzedMethod = bblock.method
        log("      performed tfa on method: " + bblock.method.toString)
      }

      // get the stack at the block entry
      var typeInfo = getTypesAtBlockEntry(bblock)

      // perform tfa to the current instruction
      log("         stack at the beginning of block " + bblock.toString + " in function " +
        bblock.method.toString + ": " + typeInfo.stack.toString)
      for (i <- 0 to (index - 1)) {
        typeInfo = tfa.interpret(typeInfo, bblock(i))
        log("         stack after interpret: " + typeInfo.stack.toString + " after instruction " +
          bblock(i).toString)
      }
      log("         stack before instruction " + index + " of block " + bblock.toString + " in function " +
        bblock.method.toString + ": " + typeInfo.stack.toString)

      // return the result
      typeInfo
    }


    /**
      * Gets the stack at the block entry. Normally the typeFlowAnalysis should be run again, but we know the stack
      * is always empty for duplicate handlers :)
      */
    def getTypesAtBlockEntry(bblock: BasicBlock): tfa.lattice.Elem = {

      // Sanity check: tfa must be done on current method
      assert(tfa.method == bblock.method)

      log("         getting typeinfo at the beginning of block " + bblock.toString)

      tfa.in.get(bblock) match {
        case Some(typeInfo) =>
            typeInfo
        case None if handlerCopiesInverted.contains(bblock) =>
          // this block was not analyzed, but it's a copy of some other block so its stack should be the same
          log("         getting typeinfo at the beginning of block " + bblock.toString + " as a copy of " +
            handlerCopiesInverted(bblock).toString)
          val typeInfo = getTypesAtBlockEntry(handlerCopiesInverted(bblock))
          while(typeInfo.stack.length > 0)
            typeInfo.stack.pop
          typeInfo
        case _ =>
          // this shouldn't happen
          sys.error("inlineExceptionHandlers optimization phase: internal error while inlining!")
      }
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
      */
    def findExceptionHandler(exception: TypeKind, handlers: List[BasicBlock]): Option[BasicBlock] = {

      // function to check if handler matches
      def handlerMatches(bb: BasicBlock): Boolean =
        if (bb.length >= 1) {
          bb.head match {
            case LOAD_EXCEPTION(clazz) if (exception <:< toTypeKind(clazz.tpe)) => true
            case _ => false
          }
        } else
          false

      // filter handlers and return the correct format
      handlers.filter(handlerMatches(_)) match {
        case handler::rest =>
          Some(handler)
        case Nil =>
          None
      }
    }


    /**
      * This function takes care of duplicating the basic block code for inlining the handler
      *
      * Note: This function does not duplicate the same basic block twice. It wil contain a map of the duplicated
      * basic blocks
      */
    def duplicateExceptionHandlerWithCaching(handler: BasicBlock): Option[(Option[Local], BasicBlock)] = {

      if (!(handlerCopies contains handler))
        handlerCopies = handlerCopies + (handler -> duplicateExceptionHandler(handler))

      handlerCopies(handler)
    }

    /** This function takes care of actual duplication */
    def duplicateExceptionHandler(handler: BasicBlock): Option[(Option[Local], BasicBlock)] = {

      log("      duplicating handler block " + handler.toString)

      // Sanitiy checks
      var canDuplicate = true
      if (handler.length < 2)                         canDuplicate = false
      if (!(handler(0).isInstanceOf[LOAD_EXCEPTION])) canDuplicate = false

      canDuplicate match {
        case true =>

          val exceptionLocal: Option[Local] =
            if (handler(1).isInstanceOf[STORE_LOCAL])
              STORE_LOCAL.unapply(handler(1).asInstanceOf[STORE_LOCAL])
            else
              None

          val dropIntstructions =
            if (exceptionLocal == None)
              1 // we only drop the LOAD_EXCEPTION and expect the exception on the stack
            else
              2 // we drop both LOAD_EXCEPTION and STORE_LOCAL

          // copy the exception handler code once again, dropping the LOAD_EXCEPTION
          val copy = handler.code.newBlock
          for (instr <- handler.drop(dropIntstructions))
            copy.emit(instr, instr.pos)
          copy.close

          // extend the handlers of the handler to the copy
          for (parentHandler <-handler.method.exh)
            if (parentHandler covers handler) {
              parentHandler.addCoveredBlock(copy)
              // notify the parent handler that the successors changed
              parentHandler.startBlock.touched = true
            }

          // notify the successors of the inlined handler might have changed
          copy.touched = true
          handler.touched = true

          log("      duplicated  handler block " + handler.toString + " to " + copy.toString)

          // announce the duplicate handler
          handlerCopiesInverted = handlerCopiesInverted + (copy -> handler)
          todoBlocks = todoBlocks + copy

          Some((exceptionLocal, copy))

        case false =>
          assert(currentClass ne null)
          currentClass.cunit.warning(NoPosition, "Unable to inline the exception handler due to incorrect format:\n" +
            handler.mkString("\n"))
          None
      }
    }
  }
}
