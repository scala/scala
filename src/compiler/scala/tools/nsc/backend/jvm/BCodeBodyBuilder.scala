/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */


package scala
package tools.nsc
package backend
package jvm

import scala.collection.{ mutable, immutable }
import scala.annotation.switch

import scala.tools.asm

/*
 *
 *  @author  Miguel Garcia, http://lamp.epfl.ch/~magarcia/ScalaCompilerCornerReloaded/
 *  @version 1.0
 *
 */
abstract class BCodeBodyBuilder extends BCodeSkelBuilder {
  import global._
  import definitions._

  /*
   * Functionality to build the body of ASM MethodNode, except for `synchronized` and `try` expressions.
   */
  abstract class PlainBodyBuilder(cunit: CompilationUnit) extends PlainSkelBuilder(cunit) {

    import icodes.TestOp
    import icodes.opcodes.InvokeStyle

    /*  If the selector type has a member with the right name,
     *  it is the host class; otherwise the symbol's owner.
     */
    def findHostClass(selector: Type, sym: Symbol) = selector member sym.name match {
      case NoSymbol   => log(s"Rejecting $selector as host class for $sym") ; sym.owner
      case _          => selector.typeSymbol
    }

    /* ---------------- helper utils for generating methods and code ---------------- */

    def emit(opc: Int) { mnode.visitInsn(opc) }
    def emit(i: asm.tree.AbstractInsnNode) { mnode.instructions.add(i) }
    def emit(is: List[asm.tree.AbstractInsnNode]) { for(i <- is) { mnode.instructions.add(i) } }

    def emitZeroOf(tk: BType) {
      (tk.sort: @switch) match {
        case asm.Type.BOOLEAN => bc.boolconst(false)
        case asm.Type.BYTE  |
             asm.Type.SHORT |
             asm.Type.CHAR  |
             asm.Type.INT     => bc.iconst(0)
        case asm.Type.LONG    => bc.lconst(0)
        case asm.Type.FLOAT   => bc.fconst(0)
        case asm.Type.DOUBLE  => bc.dconst(0)
        case asm.Type.VOID    => ()
        case _ => emit(asm.Opcodes.ACONST_NULL)
      }
    }

    /*
     * Emits code that adds nothing to the operand stack.
     * Two main cases: `tree` is an assignment,
     * otherwise an `adapt()` to UNIT is performed if needed.
     */
    def genStat(tree: Tree) {
      lineNumber(tree)
      tree match {
        case Assign(lhs @ Select(_, _), rhs) =>
          val isStatic = lhs.symbol.isStaticMember
          if (!isStatic) { genLoadQualifier(lhs) }
          genLoad(rhs, symInfoTK(lhs.symbol))
          lineNumber(tree)
          fieldStore(lhs.symbol)

        case Assign(lhs, rhs) =>
          val s = lhs.symbol
          val Local(tk, _, idx, _) = locals.getOrMakeLocal(s)
          genLoad(rhs, tk)
          lineNumber(tree)
          bc.store(idx, tk)

        case _ =>
          genLoad(tree, UNIT)
      }
    }

    def genThrow(expr: Tree): BType = {
      val thrownKind = tpeTK(expr)
      assert(exemplars.get(thrownKind).isSubtypeOf(ThrowableReference))
      genLoad(expr, thrownKind)
      lineNumber(expr)
      emit(asm.Opcodes.ATHROW) // ICode enters here into enterIgnoreMode, we'll rely instead on DCE at ClassNode level.

      RT_NOTHING // always returns the same, the invoker should know :)
    }

    /* Generate code for primitive arithmetic operations. */
    def genArithmeticOp(tree: Tree, code: Int): BType = {
      val Apply(fun @ Select(larg, _), args) = tree
      var resKind = tpeTK(larg)

      assert(resKind.isNumericType || (resKind == BOOL),
             s"$resKind is not a numeric or boolean type [operation: ${fun.symbol}]")

      import scalaPrimitives._

      args match {
        // unary operation
        case Nil =>
          genLoad(larg, resKind)
          code match {
            case POS => () // nothing
            case NEG => bc.neg(resKind)
            case NOT => bc.genPrimitiveArithmetic(icodes.NOT, resKind)
            case _ => abort(s"Unknown unary operation: ${fun.symbol.fullName} code: $code")
          }

        // binary operation
        case rarg :: Nil =>
          resKind = maxType(tpeTK(larg), tpeTK(rarg))
          if (scalaPrimitives.isShiftOp(code) || scalaPrimitives.isBitwiseOp(code)) {
            assert(resKind.isIntegralType || (resKind == BOOL),
                   s"$resKind incompatible with arithmetic modulo operation.")
          }

          genLoad(larg, resKind)
          genLoad(rarg, // check .NET size of shift arguments!
                  if (scalaPrimitives.isShiftOp(code)) INT else resKind)

          (code: @switch) match {
            case ADD => bc add resKind
            case SUB => bc sub resKind
            case MUL => bc mul resKind
            case DIV => bc div resKind
            case MOD => bc rem resKind

            case OR  | XOR | AND => bc.genPrimitiveLogical(code, resKind)

            case LSL | LSR | ASR => bc.genPrimitiveShift(code, resKind)

            case _                   => abort(s"Unknown primitive: ${fun.symbol}[$code]")
          }

        case _ =>
          abort(s"Too many arguments for primitive function: $tree")
      }
      lineNumber(tree)
      resKind
    }

    /* Generate primitive array operations. */
    def genArrayOp(tree: Tree, code: Int, expectedType: BType): BType = {
      val Apply(Select(arrayObj, _), args) = tree
      val k = tpeTK(arrayObj)
      genLoad(arrayObj, k)
      val elementType = typeOfArrayOp.getOrElse(code, abort(s"Unknown operation on arrays: $tree code: $code"))

      var generatedType = expectedType

      if (scalaPrimitives.isArrayGet(code)) {
        // load argument on stack
        assert(args.length == 1, s"Too many arguments for array get operation: $tree");
        genLoad(args.head, INT)
        generatedType = k.getComponentType
        bc.aload(elementType)
      }
      else if (scalaPrimitives.isArraySet(code)) {
        args match {
          case a1 :: a2 :: Nil =>
            genLoad(a1, INT)
            genLoad(a2)
            // the following line should really be here, but because of bugs in erasure
            // we pretend we generate whatever type is expected from us.
            //generatedType = UNIT
            bc.astore(elementType)
          case _ =>
            abort(s"Too many arguments for array set operation: $tree")
        }
      }
      else {
        generatedType = INT
        emit(asm.Opcodes.ARRAYLENGTH)
      }
      lineNumber(tree)

      generatedType
    }

    def genLoadIf(tree: If, expectedType: BType): BType = {
      val If(condp, thenp, elsep) = tree

      val success = new asm.Label
      val failure = new asm.Label

      val hasElse = !elsep.isEmpty
      val postIf  = if (hasElse) new asm.Label else failure

      genCond(condp, success, failure)

      val thenKind      = tpeTK(thenp)
      val elseKind      = if (!hasElse) UNIT else tpeTK(elsep)
      def hasUnitBranch = (thenKind == UNIT || elseKind == UNIT)
      val resKind       = if (hasUnitBranch) UNIT else tpeTK(tree)

      markProgramPoint(success)
      genLoad(thenp, resKind)
      if (hasElse) { bc goTo postIf }
      markProgramPoint(failure)
      if (hasElse) {
        genLoad(elsep, resKind)
        markProgramPoint(postIf)
      }

      resKind
    }

    def genPrimitiveOp(tree: Apply, expectedType: BType): BType = {
      val sym = tree.symbol
      val Apply(fun @ Select(receiver, _), _) = tree
      val code = scalaPrimitives.getPrimitive(sym, receiver.tpe)

      import scalaPrimitives.{isArithmeticOp, isArrayOp, isLogicalOp, isComparisonOp}

      if (isArithmeticOp(code))                genArithmeticOp(tree, code)
      else if (code == scalaPrimitives.CONCAT) genStringConcat(tree)
      else if (code == scalaPrimitives.HASH)   genScalaHash(receiver)
      else if (isArrayOp(code))                genArrayOp(tree, code, expectedType)
      else if (isLogicalOp(code) || isComparisonOp(code)) {
        val success, failure, after = new asm.Label
        genCond(tree, success, failure)
        // success block
          markProgramPoint(success)
          bc boolconst true
          bc goTo after
        // failure block
          markProgramPoint(failure)
          bc boolconst false
        // after
        markProgramPoint(after)

        BOOL
      }
      else if (code == scalaPrimitives.SYNCHRONIZED)
        genSynchronized(tree, expectedType)
      else if (scalaPrimitives.isCoercion(code)) {
        genLoad(receiver)
        lineNumber(tree)
        genCoercion(code)
        coercionTo(code)
      }
      else abort(
        s"Primitive operation not handled yet: ${sym.fullName}(${fun.symbol.simpleName}) at: ${tree.pos}"
      )
    }

    def genLoad(tree: Tree) {
      genLoad(tree, tpeTK(tree))
    }

    /* Generate code for trees that produce values on the stack */
    def genLoad(tree: Tree, expectedType: BType) {
      var generatedType = expectedType

      lineNumber(tree)

      tree match {
        case lblDf : LabelDef => genLabelDef(lblDf, expectedType)

        case ValDef(_, nme.THIS, _, _) =>
          debuglog("skipping trivial assign to _$this: " + tree)

        case ValDef(_, _, _, rhs) =>
          val sym = tree.symbol
          /* most of the time, !locals.contains(sym), unless the current activation of genLoad() is being called
             while duplicating a finalizer that contains this ValDef. */
          val Local(tk, _, idx, isSynth) = locals.getOrMakeLocal(sym)
          if (rhs == EmptyTree) { emitZeroOf(tk) }
          else { genLoad(rhs, tk) }
          bc.store(idx, tk)
          if (!isSynth) { // there are case <synthetic> ValDef's emitted by patmat
            varsInScope ::= (sym -> currProgramPoint())
          }
          generatedType = UNIT

        case t : If =>
          generatedType = genLoadIf(t, expectedType)

        case r : Return =>
          genReturn(r)
          generatedType = expectedType

        case t : Try =>
          generatedType = genLoadTry(t)

        case Throw(expr) =>
          generatedType = genThrow(expr)

        case New(tpt) =>
          abort(s"Unexpected New(${tpt.summaryString}/$tpt) reached GenBCode.\n" +
                "  Call was genLoad" + ((tree, expectedType)))

        case app : Apply =>
          generatedType = genApply(app, expectedType)

        case ApplyDynamic(qual, args) => sys.error("No invokedynamic support yet.")

        case This(qual) =>
          val symIsModuleClass = tree.symbol.isModuleClass
          assert(tree.symbol == claszSymbol || symIsModuleClass,
                 s"Trying to access the this of another class: tree.symbol = ${tree.symbol}, class symbol = $claszSymbol compilation unit: $cunit")
          if (symIsModuleClass && tree.symbol != claszSymbol) {
            generatedType = genLoadModule(tree)
          }
          else {
            mnode.visitVarInsn(asm.Opcodes.ALOAD, 0)
            generatedType =
              if (tree.symbol == ArrayClass) ObjectReference
              else brefType(thisName) // inner class (if any) for claszSymbol already tracked.
          }

        case Select(Ident(nme.EMPTY_PACKAGE_NAME), module) =>
          assert(tree.symbol.isModule, s"Selection of non-module from empty package: $tree sym: ${tree.symbol} at: ${tree.pos}")
          genLoadModule(tree)

        case Select(qualifier, selector) =>
          val sym = tree.symbol
          generatedType = symInfoTK(sym)
          val hostClass = findHostClass(qualifier.tpe, sym)
          log(s"Host class of $sym with qual $qualifier (${qualifier.tpe}) is $hostClass")
          val qualSafeToElide = treeInfo isQualifierSafeToElide qualifier

          def genLoadQualUnlessElidable() { if (!qualSafeToElide) { genLoadQualifier(tree) } }

          if (sym.isModule) {
            genLoadQualUnlessElidable()
            genLoadModule(tree)
          }
          else if (sym.isStaticMember) {
            genLoadQualUnlessElidable()
            fieldLoad(sym, hostClass)
          }
          else {
            genLoadQualifier(tree)
            fieldLoad(sym, hostClass)
          }

        case Ident(name) =>
          val sym = tree.symbol
          if (!sym.isPackage) {
            val tk = symInfoTK(sym)
            if (sym.isModule) { genLoadModule(tree) }
            else { locals.load(sym) }
            generatedType = tk
          }

        case Literal(value) =>
          if (value.tag != UnitTag) (value.tag, expectedType) match {
            case (IntTag,   LONG  ) => bc.lconst(value.longValue);       generatedType = LONG
            case (FloatTag, DOUBLE) => bc.dconst(value.doubleValue);     generatedType = DOUBLE
            case (NullTag,  _     ) => bc.emit(asm.Opcodes.ACONST_NULL); generatedType = RT_NULL
            case _                  => genConstant(value);               generatedType = tpeTK(tree)
          }

        case blck : Block => genBlock(blck, expectedType)

        case Typed(Super(_, _), _) => genLoad(This(claszSymbol), expectedType)

        case Typed(expr, _) => genLoad(expr, expectedType)

        case Assign(_, _) =>
          generatedType = UNIT
          genStat(tree)

        case av : ArrayValue =>
          generatedType = genArrayValue(av)

        case mtch : Match =>
          generatedType = genMatch(mtch)

        case EmptyTree => if (expectedType != UNIT) { emitZeroOf(expectedType) }

        case _ => abort(s"Unexpected tree in genLoad: $tree/${tree.getClass} at: ${tree.pos}")
      }

      // emit conversion
      if (generatedType != expectedType) {
        adapt(generatedType, expectedType)
      }

    } // end of GenBCode.genLoad()

    // ---------------- field load and store ----------------

    /*
     * must-single-thread
     */
    def fieldLoad( field: Symbol, hostClass: Symbol = null) {
      fieldOp(field, isLoad = true,  hostClass)
    }
    /*
     * must-single-thread
     */
    def fieldStore(field: Symbol, hostClass: Symbol = null) {
      fieldOp(field, isLoad = false, hostClass)
    }

    /*
     * must-single-thread
     */
    private def fieldOp(field: Symbol, isLoad: Boolean, hostClass: Symbol = null) {
      // LOAD_FIELD.hostClass , CALL_METHOD.hostClass , and #4283
      val owner      =
        if (hostClass == null) internalName(field.owner)
        else                  internalName(hostClass)
      val fieldJName = field.javaSimpleName.toString
      val fieldDescr = symInfoTK(field).getDescriptor
      val isStatic   = field.isStaticMember
      val opc =
        if (isLoad) { if (isStatic) asm.Opcodes.GETSTATIC else asm.Opcodes.GETFIELD }
        else        { if (isStatic) asm.Opcodes.PUTSTATIC else asm.Opcodes.PUTFIELD }
      mnode.visitFieldInsn(opc, owner, fieldJName, fieldDescr)

    }

    // ---------------- emitting constant values ----------------

    /*
     * For const.tag in {ClazzTag, EnumTag}
     *   must-single-thread
     * Otherwise it's safe to call from multiple threads.
     */
    def genConstant(const: Constant) {
      (const.tag: @switch) match {

        case BooleanTag => bc.boolconst(const.booleanValue)

        case ByteTag    => bc.iconst(const.byteValue)
        case ShortTag   => bc.iconst(const.shortValue)
        case CharTag    => bc.iconst(const.charValue)
        case IntTag     => bc.iconst(const.intValue)

        case LongTag    => bc.lconst(const.longValue)
        case FloatTag   => bc.fconst(const.floatValue)
        case DoubleTag  => bc.dconst(const.doubleValue)

        case UnitTag    => ()

        case StringTag  =>
          assert(const.value != null, const) // TODO this invariant isn't documented in `case class Constant`
          mnode.visitLdcInsn(const.stringValue) // `stringValue` special-cases null, but not for a const with StringTag

        case NullTag    => emit(asm.Opcodes.ACONST_NULL)

        case ClazzTag   =>
          val toPush: BType = {
            val kind = toTypeKind(const.typeValue)
            if (kind.isValueType) classLiteral(kind)
            else kind
          }
          mnode.visitLdcInsn(toPush.toASMType)

        case EnumTag   =>
          val sym       = const.symbolValue
          val ownerName = internalName(sym.owner)
          val fieldName = sym.javaSimpleName.toString
          val fieldDesc = toTypeKind(sym.tpe.underlying).getDescriptor
          mnode.visitFieldInsn(
            asm.Opcodes.GETSTATIC,
            ownerName,
            fieldName,
            fieldDesc
          )

        case _ => abort(s"Unknown constant value: $const")
      }
    }

    private def genLabelDef(lblDf: LabelDef, expectedType: BType) {
      // duplication of LabelDefs contained in `finally`-clauses is handled when emitting RETURN. No bookkeeping for that required here.
      // no need to call index() over lblDf.params, on first access that magic happens (moreover, no LocalVariableTable entries needed for them).
      markProgramPoint(programPoint(lblDf.symbol))
      lineNumber(lblDf)
      genLoad(lblDf.rhs, expectedType)
    }

    private def genReturn(r: Return) {
      val Return(expr) = r
      val returnedKind = tpeTK(expr)
      genLoad(expr, returnedKind)
      adapt(returnedKind, returnType)
      val saveReturnValue = (returnType != UNIT)
      lineNumber(r)

      cleanups match {
        case Nil =>
          // not an assertion: !shouldEmitCleanup (at least not yet, pendingCleanups() may still have to run, and reset `shouldEmitCleanup`.
          bc emitRETURN returnType
        case nextCleanup :: rest =>
          if (saveReturnValue) {
            if (insideCleanupBlock) {
              cunit.warning(r.pos, "Return statement found in finally-clause, discarding its return-value in favor of that of a more deeply nested return.")
              bc drop returnType
            } else {
              // regarding return value, the protocol is: in place of a `return-stmt`, a sequence of `adapt, store, jump` are inserted.
              if (earlyReturnVar == null) {
                earlyReturnVar = locals.makeLocal(returnType, "earlyReturnVar")
              }
              locals.store(earlyReturnVar)
            }
          }
          bc goTo nextCleanup
          shouldEmitCleanup = true
      }

    } // end of genReturn()

    private def genApply(app: Apply, expectedType: BType): BType = {
      var generatedType = expectedType
      lineNumber(app)
      app match {

        case Apply(TypeApply(fun, targs), _) =>

          val sym = fun.symbol
          val cast = sym match {
            case Object_isInstanceOf  => false
            case Object_asInstanceOf  => true
            case _                    => abort(s"Unexpected type application $fun[sym: ${sym.fullName}] in: $app")
          }

          val Select(obj, _) = fun
          val l = tpeTK(obj)
          val r = tpeTK(targs.head)

          def genTypeApply(): BType = {
            genLoadQualifier(fun)

            if (l.isValueType && r.isValueType)
              genConversion(l, r, cast)
            else if (l.isValueType) {
              bc drop l
              if (cast) {
                mnode.visitTypeInsn(asm.Opcodes.NEW, classCastExceptionReference.getInternalName)
                bc dup ObjectReference
                emit(asm.Opcodes.ATHROW)
              } else {
                bc boolconst false
              }
            }
            else if (r.isValueType && cast) {
              abort(s"Erasure should have added an unboxing operation to prevent this cast. Tree: $app")
            }
            else if (r.isValueType) {
              bc isInstance classLiteral(r)
            }
            else {
              genCast(r, cast)
            }

            if (cast) r else BOOL
          } // end of genTypeApply()

          generatedType = genTypeApply()

        // 'super' call: Note: since constructors are supposed to
        // return an instance of what they construct, we have to take
        // special care. On JVM they are 'void', and Scala forbids (syntactically)
        // to call super constructors explicitly and/or use their 'returned' value.
        // therefore, we can ignore this fact, and generate code that leaves nothing
        // on the stack (contrary to what the type in the AST says).
        case Apply(fun @ Select(Super(_, mix), _), args) =>
          val invokeStyle = icodes.opcodes.SuperCall(mix)
          // if (fun.symbol.isConstructor) Static(true) else SuperCall(mix);
          mnode.visitVarInsn(asm.Opcodes.ALOAD, 0)
          genLoadArguments(args, paramTKs(app))
          genCallMethod(fun.symbol, invokeStyle, pos = app.pos)
          generatedType = asmMethodType(fun.symbol).getReturnType

        // 'new' constructor call: Note: since constructors are
        // thought to return an instance of what they construct,
        // we have to 'simulate' it by DUPlicating the freshly created
        // instance (on JVM, <init> methods return VOID).
        case Apply(fun @ Select(New(tpt), nme.CONSTRUCTOR), args) =>
          val ctor = fun.symbol
          assert(ctor.isClassConstructor, s"'new' call to non-constructor: ${ctor.name}")

          generatedType = tpeTK(tpt)
          assert(generatedType.isRefOrArrayType, s"Non reference type cannot be instantiated: $generatedType")

          generatedType match {
            case arr if generatedType.isArray =>
              genLoadArguments(args, paramTKs(app))
              val dims     = arr.getDimensions
              var elemKind = arr.getElementType
              val argsSize = args.length
              if (argsSize > dims) {
                cunit.error(app.pos, s"too many arguments for array constructor: found ${args.length} but array has only $dims dimension(s)")
              }
              if (argsSize < dims) {
                /* In one step:
                 *   elemKind = new BType(BType.ARRAY, arr.off + argsSize, arr.len - argsSize)
                 * however the above does not enter a TypeName for each nested arrays in chrs.
                 */
                for (i <- args.length until dims) elemKind = arrayOf(elemKind)
              }
              (argsSize : @switch) match {
                case 1 => bc newarray elemKind
                case _ =>
                  val descr = ('[' * argsSize) + elemKind.getDescriptor // denotes the same as: arrayN(elemKind, argsSize).getDescriptor
                  mnode.visitMultiANewArrayInsn(descr, argsSize)
              }

            case rt if generatedType.hasObjectSort =>
              assert(exemplar(ctor.owner).c == rt, s"Symbol ${ctor.owner.fullName} is different from $rt")
              mnode.visitTypeInsn(asm.Opcodes.NEW, rt.getInternalName)
              bc dup generatedType
              genLoadArguments(args, paramTKs(app))
              genCallMethod(ctor, icodes.opcodes.Static(onInstance = true))

            case _ =>
              abort(s"Cannot instantiate $tpt of kind: $generatedType")
          }

        case Apply(fun @ _, List(expr)) if definitions.isBox(fun.symbol) =>
          val nativeKind = tpeTK(expr)
          genLoad(expr, nativeKind)
          val MethodNameAndType(mname, mdesc) = asmBoxTo(nativeKind)
          bc.invokestatic(BoxesRunTime.getInternalName, mname, mdesc)
          generatedType = boxResultType(fun.symbol) // was toTypeKind(fun.symbol.tpe.resultType)

        case Apply(fun @ _, List(expr)) if definitions.isUnbox(fun.symbol) =>
          genLoad(expr)
          val boxType = unboxResultType(fun.symbol) // was toTypeKind(fun.symbol.owner.linkedClassOfClass.tpe)
          generatedType = boxType
          val MethodNameAndType(mname, mdesc) = asmUnboxTo(boxType)
          bc.invokestatic(BoxesRunTime.getInternalName, mname, mdesc)

        case app @ Apply(fun, args) =>
          val sym = fun.symbol

          if (sym.isLabel) {  // jump to a label
            genLoadLabelArguments(args, labelDef(sym), app.pos)
            bc goTo programPoint(sym)
          } else if (isPrimitive(sym)) { // primitive method call
            generatedType = genPrimitiveOp(app, expectedType)
          } else {  // normal method call

            def genNormalMethodCall() {

              val invokeStyle =
                if (sym.isStaticMember) icodes.opcodes.Static(onInstance = false)
                else if (sym.isPrivate || sym.isClassConstructor) icodes.opcodes.Static(onInstance = true)
                else icodes.opcodes.Dynamic;

              if (invokeStyle.hasInstance) {
                genLoadQualifier(fun)
              }

              genLoadArguments(args, paramTKs(app))

              // In "a couple cases", squirrel away a extra information (hostClass, targetTypeKind). TODO Document what "in a couple cases" refers to.
              var hostClass:      Symbol = null
              var targetTypeKind: BType  = null
              fun match {
                case Select(qual, _) =>
                  val qualSym = findHostClass(qual.tpe, sym)
                  if (qualSym == ArrayClass) {
                    targetTypeKind = tpeTK(qual)
                    log(s"Stored target type kind for ${sym.fullName} as $targetTypeKind")
                  }
                  else {
                    hostClass = qualSym
                    if (qual.tpe.typeSymbol != qualSym) {
                      log(s"Precisified host class for $sym from ${qual.tpe.typeSymbol.fullName} to ${qualSym.fullName}")
                    }
                  }

                case _ =>
              }
              if ((targetTypeKind != null) && (sym == definitions.Array_clone) && invokeStyle.isDynamic) {
                val target: String = targetTypeKind.getInternalName
                bc.invokevirtual(target, "clone", "()Ljava/lang/Object;")
              }
              else {
                genCallMethod(sym, invokeStyle, hostClass, app.pos)
              }

            } // end of genNormalMethodCall()

            genNormalMethodCall()

            generatedType = asmMethodType(sym).getReturnType
          }

      }

      generatedType
    } // end of genApply()

    private def genArrayValue(av: ArrayValue): BType = {
      val ArrayValue(tpt @ TypeTree(), elems) = av

      val elmKind       = tpeTK(tpt)
      val generatedType = arrayOf(elmKind)

      lineNumber(av)
      bc iconst   elems.length
      bc newarray elmKind

      var i = 0
      var rest = elems
      while (!rest.isEmpty) {
        bc dup     generatedType
        bc iconst  i
        genLoad(rest.head, elmKind)
        bc astore  elmKind
        rest = rest.tail
        i = i + 1
      }

      generatedType
    }

    /*
     * A Match node contains one or more case clauses,
     * each case clause lists one or more Int values to use as keys, and a code block.
     * Except the "default" case clause which (if it exists) doesn't list any Int key.
     *
     * On a first pass over the case clauses, we flatten the keys and their targets (the latter represented with asm.Labels).
     * That representation allows JCodeMethodV to emit a lookupswitch or a tableswitch.
     *
     * On a second pass, we emit the switch blocks, one for each different target.
     */
    private def genMatch(tree: Match): BType = {
      lineNumber(tree)
      genLoad(tree.selector, INT)
      val generatedType = tpeTK(tree)

      var flatKeys: List[Int]       = Nil
      var targets:  List[asm.Label] = Nil
      var default:  asm.Label       = null
      var switchBlocks: List[Pair[asm.Label, Tree]] = Nil

      // collect switch blocks and their keys, but don't emit yet any switch-block.
      for (caze @ CaseDef(pat, guard, body) <- tree.cases) {
        assert(guard == EmptyTree, guard)
        val switchBlockPoint = new asm.Label
        switchBlocks ::= Pair(switchBlockPoint, body)
        pat match {
          case Literal(value) =>
            flatKeys ::= value.intValue
            targets  ::= switchBlockPoint
          case Ident(nme.WILDCARD) =>
            assert(default == null, s"multiple default targets in a Match node, at ${tree.pos}")
            default = switchBlockPoint
          case Alternative(alts) =>
            alts foreach {
              case Literal(value) =>
                flatKeys ::= value.intValue
                targets  ::= switchBlockPoint
              case _ =>
                abort(s"Invalid alternative in alternative pattern in Match node: $tree at: ${tree.pos}")
            }
          case _ =>
            abort(s"Invalid pattern in Match node: $tree at: ${tree.pos}")
        }
      }
      bc.emitSWITCH(mkArrayReverse(flatKeys), mkArray(targets.reverse), default, MIN_SWITCH_DENSITY)

      // emit switch-blocks.
      val postMatch = new asm.Label
      for (sb <- switchBlocks.reverse) {
        val Pair(caseLabel, caseBody) = sb
        markProgramPoint(caseLabel)
        genLoad(caseBody, generatedType)
        bc goTo postMatch
      }

      markProgramPoint(postMatch)
      generatedType
    }

    def genBlock(tree: Block, expectedType: BType) {
      val Block(stats, expr) = tree
      val savedScope = varsInScope
      varsInScope = Nil
      stats foreach genStat
      genLoad(expr, expectedType)
      val end = currProgramPoint()
      if (emitVars) { // add entries to LocalVariableTable JVM attribute
        for (Pair(sym, start) <- varsInScope.reverse) { emitLocalVarScope(sym, start, end) }
      }
      varsInScope = savedScope
    }

    def adapt(from: BType, to: BType) {
      if (!conforms(from, to)) {
        to match {
          case UNIT => bc drop from
          case _    => bc.emitT2T(from, to)
        }
      } else if (from.isNothingType) {
        emit(asm.Opcodes.ATHROW) // ICode enters here into enterIgnoreMode, we'll rely instead on DCE at ClassNode level.
      } else if (from.isNullType) {
        bc drop from
        mnode.visitInsn(asm.Opcodes.ACONST_NULL)
      }
      else (from, to) match  {
        case (BYTE, LONG) | (SHORT, LONG) | (CHAR, LONG) | (INT, LONG) => bc.emitT2T(INT, LONG)
        case _ => ()
      }
    }

    /* Emit code to Load the qualifier of `tree` on top of the stack. */
    def genLoadQualifier(tree: Tree) {
      lineNumber(tree)
      tree match {
        case Select(qualifier, _) => genLoad(qualifier)
        case _                    => abort(s"Unknown qualifier $tree")
      }
    }

    /* Generate code that loads args into label parameters. */
    def genLoadLabelArguments(args: List[Tree], lblDef: LabelDef, gotoPos: Position) {
      assert(args forall { a => !a.hasSymbolField || a.hasSymbolWhich( s => !s.isLabel) }, s"SI-6089 at: $gotoPos") // SI-6089

      val aps = {
        val params: List[Symbol] = lblDef.params.map(_.symbol)
        assert(args.length == params.length, s"Wrong number of arguments in call to label at: $gotoPos")

        def isTrivial(kv: (Tree, Symbol)) = kv match {
          case (This(_), p) if p.name == nme.THIS     => true
          case (arg @ Ident(_), p) if arg.symbol == p => true
          case _                                      => false
        }

        (args zip params) filterNot isTrivial
      }

      // first push *all* arguments. This makes sure muliple uses of the same labelDef-var will all denote the (previous) value.
      aps foreach { case (arg, param) => genLoad(arg, locals(param).tk) } // `locals` is known to contain `param` because `genDefDef()` visited `labelDefsAtOrUnder`

      // second assign one by one to the LabelDef's variables.
      aps.reverse foreach {
        case (_, param) =>
          // TODO FIXME a "this" param results from tail-call xform. If so, the `else` branch seems perfectly fine. And the `then` branch must be wrong.
          if (param.name == nme.THIS) mnode.visitVarInsn(asm.Opcodes.ASTORE, 0)
          else locals.store(param)
      }

    }

    def genLoadArguments(args: List[Tree], btpes: List[BType]) {
      (args zip btpes) foreach { case (arg, btpe) => genLoad(arg, btpe) }
    }

    def genLoadModule(tree: Tree): BType = {
      // Working around SI-5604.  Rather than failing the compile when we see a package here, check if there's a package object.
      val module = (
        if (!tree.symbol.isPackageClass) tree.symbol
        else tree.symbol.info.member(nme.PACKAGE) match {
          case NoSymbol => abort(s"Cannot use package as value: $tree") ; NoSymbol
          case s        => devWarning("Bug: found package class where package object expected.  Converting.") ; s.moduleClass
        }
      )
      lineNumber(tree)
      genLoadModule(module)
      symInfoTK(module)
    }

    def genLoadModule(module: Symbol) {
      if (claszSymbol == module.moduleClass && jMethodName != "readResolve") {
        mnode.visitVarInsn(asm.Opcodes.ALOAD, 0)
      } else {
        val mbt  = symInfoTK(module)
        mnode.visitFieldInsn(
          asm.Opcodes.GETSTATIC,
          mbt.getInternalName /* + "$" */ ,
          strMODULE_INSTANCE_FIELD,
          mbt.getDescriptor // for nostalgics: toTypeKind(module.tpe).getDescriptor
        )
      }
    }

    def genConversion(from: BType, to: BType, cast: Boolean) {
      if (cast) { bc.emitT2T(from, to) }
      else {
        bc drop from
        bc boolconst (from == to)
      }
    }

    def genCast(to: BType, cast: Boolean) {
      if (cast) { bc checkCast  to }
      else      { bc isInstance to }
    }

    /* Is the given symbol a primitive operation? */
    def isPrimitive(fun: Symbol): Boolean = scalaPrimitives.isPrimitive(fun)

    /* Generate coercion denoted by "code" */
    def genCoercion(code: Int) {
      import scalaPrimitives._
      (code: @switch) match {
        case B2B | S2S | C2C | I2I | L2L | F2F | D2D => ()
        case _ =>
          val from = coercionFrom(code)
          val to   = coercionTo(code)
          bc.emitT2T(from, to)
      }
    }

    def genStringConcat(tree: Tree): BType = {
      lineNumber(tree)
      liftStringConcat(tree) match {

        // Optimization for expressions of the form "" + x.  We can avoid the StringBuilder.
        case List(Literal(Constant("")), arg) =>
          genLoad(arg, ObjectReference)
          genCallMethod(String_valueOf, icodes.opcodes.Static(onInstance = false))

        case concatenations =>
          bc.genStartConcat
          for (elem <- concatenations) {
            val kind = tpeTK(elem)
            genLoad(elem, kind)
            bc.genStringConcat(kind)
          }
          bc.genEndConcat

      }

      StringReference
    }

    def genCallMethod(method: Symbol, style: InvokeStyle, hostClass0: Symbol = null, pos: Position = NoPosition) {

      val siteSymbol = claszSymbol
      val hostSymbol = if (hostClass0 == null) method.owner else hostClass0;
      val methodOwner = method.owner
      // info calls so that types are up to date; erasure may add lateINTERFACE to traits
      hostSymbol.info ; methodOwner.info

      def needsInterfaceCall(sym: Symbol) = (
           sym.isInterface
        || sym.isJavaDefined && sym.isNonBottomSubClass(definitions.ClassfileAnnotationClass)
      )

      def isAccessibleFrom(target: Symbol, site: Symbol): Boolean = {
        target.isPublic || target.isProtected && {
          (site.enclClass isSubClass target.enclClass) ||
          (site.enclosingPackage == target.privateWithin)
        }
      }

      // whether to reference the type of the receiver or
      // the type of the method owner
      val useMethodOwner = (
           style != icodes.opcodes.Dynamic
        || hostSymbol.isBottomClass
        || methodOwner == definitions.ObjectClass
      )
      val receiver = if (useMethodOwner) methodOwner else hostSymbol
      val bmOwner  = asmClassType(receiver)
      val jowner   = bmOwner.getInternalName
      val jname    = method.javaSimpleName.toString
      val bmType   = asmMethodType(method)
      val mdescr   = bmType.getDescriptor

      def initModule() {
        // we initialize the MODULE$ field immediately after the super ctor
        if (!isModuleInitialized &&
            jMethodName == INSTANCE_CONSTRUCTOR_NAME &&
            jname == INSTANCE_CONSTRUCTOR_NAME &&
            isStaticModule(siteSymbol)) {
          isModuleInitialized = true
          mnode.visitVarInsn(asm.Opcodes.ALOAD, 0)
          mnode.visitFieldInsn(
            asm.Opcodes.PUTSTATIC,
            thisName,
            strMODULE_INSTANCE_FIELD,
            "L" + thisName + ";"
          )
        }
      }

      if (style.isStatic) {
        if (style.hasInstance) { bc.invokespecial  (jowner, jname, mdescr) }
        else                   { bc.invokestatic   (jowner, jname, mdescr) }
      }
      else if (style.isDynamic) {
        if (needsInterfaceCall(receiver)) { bc.invokeinterface(jowner, jname, mdescr) }
        else                              { bc.invokevirtual  (jowner, jname, mdescr) }
      }
      else {
        assert(style.isSuper, s"An unknown InvokeStyle: $style")
        bc.invokespecial(jowner, jname, mdescr)
        initModule()
      }

    } // end of genCallMethod()

    /* Generate the scala ## method. */
    def genScalaHash(tree: Tree): BType = {
      genLoadModule(ScalaRunTimeModule) // TODO why load ScalaRunTimeModule if ## has InvokeStyle of Static(false) ?
      genLoad(tree, ObjectReference)
      genCallMethod(hashMethodSym, icodes.opcodes.Static(onInstance = false))

      INT
    }

    /*
     * Returns a list of trees that each should be concatenated, from left to right.
     * It turns a chained call like "a".+("b").+("c") into a list of arguments.
     */
    def liftStringConcat(tree: Tree): List[Tree] = tree match {
      case Apply(fun @ Select(larg, method), rarg) =>
        if (isPrimitive(fun.symbol) &&
            scalaPrimitives.getPrimitive(fun.symbol) == scalaPrimitives.CONCAT)
          liftStringConcat(larg) ::: rarg
        else
          tree :: Nil
      case _ =>
        tree :: Nil
    }

    /* Some useful equality helpers. */
    def isNull(t: Tree) = {
      t match {
        case Literal(Constant(null)) => true
        case _ => false
      }
    }

    /* If l or r is constant null, returns the other ; otherwise null */
    def ifOneIsNull(l: Tree, r: Tree) = if (isNull(l)) r else if (isNull(r)) l else null

    /* Emit code to compare the two top-most stack values using the 'op' operator. */
    private def genCJUMP(success: asm.Label, failure: asm.Label, op: TestOp, tk: BType) {
      if (tk.isIntSizedType) { // BOOL, BYTE, CHAR, SHORT, or INT
        bc.emitIF_ICMP(op, success)
      } else if (tk.isRefOrArrayType) { // REFERENCE(_) | ARRAY(_)
        bc.emitIF_ACMP(op, success)
      } else {
        (tk: @unchecked) match {
          case LONG   => emit(asm.Opcodes.LCMP)
          case FLOAT  =>
            if (op == icodes.LT || op == icodes.LE) emit(asm.Opcodes.FCMPG)
            else emit(asm.Opcodes.FCMPL)
          case DOUBLE =>
            if (op == icodes.LT || op == icodes.LE) emit(asm.Opcodes.DCMPG)
            else emit(asm.Opcodes.DCMPL)
        }
        bc.emitIF(op, success)
      }
      bc goTo failure
    }

    /* Emits code to compare (and consume) stack-top and zero using the 'op' operator */
    private def genCZJUMP(success: asm.Label, failure: asm.Label, op: TestOp, tk: BType) {
      if (tk.isIntSizedType) { // BOOL, BYTE, CHAR, SHORT, or INT
        bc.emitIF(op, success)
      } else if (tk.isRefOrArrayType) { // REFERENCE(_) | ARRAY(_)
        // @unchecked because references aren't compared with GT, GE, LT, LE.
        (op : @unchecked) match {
          case icodes.EQ => bc emitIFNULL    success
          case icodes.NE => bc emitIFNONNULL success
        }
      } else {
        (tk: @unchecked) match {
          case LONG   =>
            emit(asm.Opcodes.LCONST_0)
            emit(asm.Opcodes.LCMP)
          case FLOAT  =>
            emit(asm.Opcodes.FCONST_0)
            if (op == icodes.LT || op == icodes.LE) emit(asm.Opcodes.FCMPG)
            else emit(asm.Opcodes.FCMPL)
          case DOUBLE =>
            emit(asm.Opcodes.DCONST_0)
            if (op == icodes.LT || op == icodes.LE) emit(asm.Opcodes.DCMPG)
            else emit(asm.Opcodes.DCMPL)
        }
        bc.emitIF(op, success)
      }
      bc goTo failure
    }

    val testOpForPrimitive: Array[TestOp] = Array(
      icodes.EQ, icodes.NE, icodes.EQ, icodes.NE, icodes.LT, icodes.LE, icodes.GE, icodes.GT
    )

    /*
     * Generate code for conditional expressions.
     * The jump targets success/failure of the test are `then-target` and `else-target` resp.
     */
    private def genCond(tree: Tree, success: asm.Label, failure: asm.Label) {

      def genComparisonOp(l: Tree, r: Tree, code: Int) {
        val op: TestOp = testOpForPrimitive(code - scalaPrimitives.ID)
        // special-case reference (in)equality test for null (null eq x, x eq null)
        var nonNullSide: Tree = null
        if (scalaPrimitives.isReferenceEqualityOp(code) &&
            { nonNullSide = ifOneIsNull(l, r); nonNullSide != null }
        ) {
          genLoad(nonNullSide, ObjectReference)
          genCZJUMP(success, failure, op, ObjectReference)
        }
        else {
          val tk = maxType(tpeTK(l), tpeTK(r))
          genLoad(l, tk)
          genLoad(r, tk)
          genCJUMP(success, failure, op, tk)
        }
      }

      def default() = {
        genLoad(tree, BOOL)
        genCZJUMP(success, failure, icodes.NE, BOOL)
      }

      lineNumber(tree)
      tree match {

        case Apply(fun, args) if isPrimitive(fun.symbol) =>
          import scalaPrimitives.{ ZNOT, ZAND, ZOR, EQ, getPrimitive }

          // lhs and rhs of test
          lazy val Select(lhs, _) = fun
          val rhs = if (args.isEmpty) EmptyTree else args.head; // args.isEmpty only for ZNOT

          def genZandOrZor(and: Boolean) { // TODO WRONG
            // reaching "keepGoing" indicates the rhs should be evaluated too (ie not short-circuited).
            val keepGoing = new asm.Label

            if (and) genCond(lhs, keepGoing, failure)
            else     genCond(lhs, success,   keepGoing)

            markProgramPoint(keepGoing)
            genCond(rhs, success, failure)
          }

          getPrimitive(fun.symbol) match {
            case ZNOT   => genCond(lhs, failure, success)
            case ZAND   => genZandOrZor(and = true)
            case ZOR    => genZandOrZor(and = false)
            case code   =>
              // TODO !!!!!!!!!! isReferenceType, in the sense of TypeKind? (ie non-array, non-boxed, non-nothing, may be null)
              if (scalaPrimitives.isUniversalEqualityOp(code) && tpeTK(lhs).hasObjectSort) {
                // `lhs` has reference type
                if (code == EQ) genEqEqPrimitive(lhs, rhs, success, failure)
                else            genEqEqPrimitive(lhs, rhs, failure, success)
              }
              else if (scalaPrimitives.isComparisonOp(code))
                genComparisonOp(lhs, rhs, code)
              else
                default
          }

        case _ => default
      }

    } // end of genCond()

    /*
     * Generate the "==" code for object references. It is equivalent of
     * if (l eq null) r eq null else l.equals(r);
     *
     * @param l       left-hand-side  of the '=='
     * @param r       right-hand-side of the '=='
     */
    def genEqEqPrimitive(l: Tree, r: Tree, success: asm.Label, failure: asm.Label) {

      /* True if the equality comparison is between values that require the use of the rich equality
       * comparator (scala.runtime.Comparator.equals). This is the case when either side of the
       * comparison might have a run-time type subtype of java.lang.Number or java.lang.Character.
       * When it is statically known that both sides are equal and subtypes of Number of Character,
       * not using the rich equality is possible (their own equals method will do ok.)
       */
      val mustUseAnyComparator: Boolean = {
        val areSameFinals = l.tpe.isFinalType && r.tpe.isFinalType && (l.tpe =:= r.tpe)

        !areSameFinals && platform.isMaybeBoxed(l.tpe.typeSymbol) && platform.isMaybeBoxed(r.tpe.typeSymbol)
      }

      if (mustUseAnyComparator) {
        val equalsMethod = {

          def default = platform.externalEquals

          platform match {
            case x: JavaPlatform =>
              import x._
                if (l.tpe <:< BoxedNumberClass.tpe) {
                  if (r.tpe <:< BoxedNumberClass.tpe) externalEqualsNumNum
                  else if (r.tpe <:< BoxedCharacterClass.tpe) externalEqualsNumChar
                  else externalEqualsNumObject
                }
                else default

            case _ => default
          }
        }
        genLoad(l, ObjectReference)
        genLoad(r, ObjectReference)
        genCallMethod(equalsMethod, icodes.opcodes.Static(onInstance = false))
        genCZJUMP(success, failure, icodes.NE, BOOL)
      }
      else {
        if (isNull(l)) {
          // null == expr -> expr eq null
          genLoad(r, ObjectReference)
          genCZJUMP(success, failure, icodes.EQ, ObjectReference)
        } else if (isNull(r)) {
          // expr == null -> expr eq null
          genLoad(l, ObjectReference)
          genCZJUMP(success, failure, icodes.EQ, ObjectReference)
        } else {
          // l == r -> if (l eq null) r eq null else l.equals(r)
          val eqEqTempLocal = locals.makeLocal(AnyRefReference, nme.EQEQ_LOCAL_VAR.toString)
          val lNull    = new asm.Label
          val lNonNull = new asm.Label

          genLoad(l, ObjectReference)
          genLoad(r, ObjectReference)
          locals.store(eqEqTempLocal)
          bc dup ObjectReference
          genCZJUMP(lNull, lNonNull, icodes.EQ, ObjectReference)

          markProgramPoint(lNull)
          bc drop ObjectReference
          locals.load(eqEqTempLocal)
          genCZJUMP(success, failure, icodes.EQ, ObjectReference)

          markProgramPoint(lNonNull)
          locals.load(eqEqTempLocal)
          genCallMethod(Object_equals, icodes.opcodes.Dynamic)
          genCZJUMP(success, failure, icodes.NE, BOOL)
        }
      }
    }

    /* can-multi-thread */
    def getMaxType(ts: List[Type]): BType = {
      ts map toTypeKind reduceLeft maxType
    }

    def genSynchronized(tree: Apply, expectedType: BType): BType
    def genLoadTry(tree: Try): BType

  }

}
