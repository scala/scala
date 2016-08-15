/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */


package scala
package tools.nsc
package backend
package jvm

import scala.annotation.switch
import scala.reflect.internal.Flags
import scala.tools.asm
import GenBCode._
import BackendReporting._
import scala.collection.mutable
import scala.tools.asm.Opcodes
import scala.tools.asm.tree.{MethodInsnNode, MethodNode}
import scala.tools.nsc.backend.jvm.BCodeHelpers.{InvokeStyle, TestOp}

/*
 *
 *  @author  Miguel Garcia, http://lamp.epfl.ch/~magarcia/ScalaCompilerCornerReloaded/
 *  @version 1.0
 *
 */
abstract class BCodeBodyBuilder extends BCodeSkelBuilder {
  import global._
  import definitions._
  import bTypes._
  import coreBTypes._

  /*
   * Functionality to build the body of ASM MethodNode, except for `synchronized` and `try` expressions.
   */
  abstract class PlainBodyBuilder(cunit: CompilationUnit) extends PlainSkelBuilder(cunit) {
    /* ---------------- helper utils for generating methods and code ---------------- */

    def emit(opc: Int) { mnode.visitInsn(opc) }

    def emitZeroOf(tk: BType) {
      tk match {
        case BOOL => bc.boolconst(false)
        case BYTE  |
             SHORT |
             CHAR  |
             INT     => bc.iconst(0)
        case LONG    => bc.lconst(0)
        case FLOAT   => bc.fconst(0)
        case DOUBLE  => bc.dconst(0)
        case UNIT    => ()
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
        case Assign(lhs @ Select(qual, _), rhs) =>
          val isStatic = lhs.symbol.isStaticMember
          if (!isStatic) { genLoadQualifier(lhs) }
          genLoad(rhs, symInfoTK(lhs.symbol))
          lineNumber(tree)
          // receiverClass is used in the bytecode to access the field. using sym.owner may lead to IllegalAccessError, SI-4283
          val receiverClass = qual.tpe.typeSymbol
          fieldStore(lhs.symbol, receiverClass)

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
      // `throw null` is valid although scala.Null (as defined in src/library-aux) isn't a subtype of Throwable.
      // Similarly for scala.Nothing (again, as defined in src/library-aux).
      assert(thrownKind.isNullType || thrownKind.isNothingType || thrownKind.asClassBType.isSubtypeOf(jlThrowableRef).get)
      genLoad(expr, thrownKind)
      lineNumber(expr)
      emit(asm.Opcodes.ATHROW) // ICode enters here into enterIgnoreMode, we'll rely instead on DCE at ClassNode level.

      srNothingRef // always returns the same, the invoker should know :)
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
            case NOT => bc.genPrimitiveNot(resKind)
            case _ => abort(s"Unknown unary operation: ${fun.symbol.fullName} code: $code")
          }

        // binary operation
        case rarg :: Nil =>
          val isShiftOp = scalaPrimitives.isShiftOp(code)
          resKind = tpeTK(larg).maxType(if (isShiftOp) INT else tpeTK(rarg))

          if (isShiftOp || scalaPrimitives.isBitwiseOp(code)) {
            assert(resKind.isIntegralType || (resKind == BOOL),
                   s"$resKind incompatible with arithmetic modulo operation.")
          }

          genLoad(larg, resKind)
          genLoad(rarg, if (isShiftOp) INT else resKind)

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
        generatedType = k.asArrayBType.componentType
        bc.aload(elementType)
      } else if (scalaPrimitives.isArraySet(code)) {
        val List(a1, a2) = args
        genLoad(a1, INT)
        genLoad(a2)
        generatedType = UNIT
        bc.astore(elementType)
      } else {
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

      genCond(condp, success, failure, targetIfNoJump = success)
      markProgramPoint(success)

      val thenKind      = tpeTK(thenp)
      val elseKind      = if (!hasElse) UNIT else tpeTK(elsep)
      def hasUnitBranch = (thenKind == UNIT || elseKind == UNIT)
      val resKind       = if (hasUnitBranch) UNIT else tpeTK(tree)

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
      else if (code == scalaPrimitives.HASH)   genScalaHash(receiver, tree.pos)
      else if (isArrayOp(code))                genArrayOp(tree, code, expectedType)
      else if (isLogicalOp(code) || isComparisonOp(code)) {
        val success, failure, after = new asm.Label
        genCond(tree, success, failure, targetIfNoJump = success)
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
          val localVarStart = currProgramPoint()
          bc.store(idx, tk)
          if (!isSynth) { // there are case <synthetic> ValDef's emitted by patmat
            varsInScope ::= (sym -> localVarStart)
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

        case app @ ApplyDynamic(qual, Literal(Constant(boostrapMethodRef: Symbol)) :: staticAndDynamicArgs) =>
          val numStaticArgs = boostrapMethodRef.paramss.head.size - 3 /*JVM provided args*/
          val (staticArgs, dynamicArgs) = staticAndDynamicArgs.splitAt(numStaticArgs)
          val boostrapDescriptor = staticHandleFromSymbol(boostrapMethodRef)
          val bootstrapArgs = staticArgs.map({case t @ Literal(c: Constant) => bootstrapMethodArg(c, t.pos)})
          val descriptor = methodBTypeFromMethodType(qual.symbol.info, false)
          genLoadArguments(dynamicArgs, qual.symbol.info.params.map(param => typeToBType(param.info)))
          mnode.visitInvokeDynamicInsn(qual.symbol.name.encoded, descriptor.descriptor, boostrapDescriptor, bootstrapArgs : _*)

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
              if (tree.symbol == ArrayClass) ObjectRef
              else classBTypeFromSymbol(claszSymbol)
          }

        case Select(Ident(nme.EMPTY_PACKAGE_NAME), module) =>
          assert(tree.symbol.isModule, s"Selection of non-module from empty package: $tree sym: ${tree.symbol} at: ${tree.pos}")
          genLoadModule(tree)

        case Select(qualifier, _) =>
          val sym = tree.symbol
          generatedType = symInfoTK(sym)
          val qualSafeToElide = treeInfo isQualifierSafeToElide qualifier
          def genLoadQualUnlessElidable() { if (!qualSafeToElide) { genLoadQualifier(tree) } }
          // receiverClass is used in the bytecode to access the field. using sym.owner may lead to IllegalAccessError, SI-4283
          def receiverClass = qualifier.tpe.typeSymbol
          if (sym.isModule) {
            genLoadQualUnlessElidable()
            genLoadModule(tree)
          } else if (sym.isStaticMember) {
            genLoadQualUnlessElidable()
            fieldLoad(sym, receiverClass)
          } else {
            genLoadQualifier(tree)
            fieldLoad(sym, receiverClass)
          }

        case Ident(name) =>
          val sym = tree.symbol
          if (!sym.hasPackageFlag) {
            val tk = symInfoTK(sym)
            if (sym.isModule) { genLoadModule(tree) }
            else { locals.load(sym) }
            generatedType = tk
          }

        case Literal(value) =>
          if (value.tag != UnitTag) (value.tag, expectedType) match {
            case (IntTag,   LONG  ) => bc.lconst(value.longValue);       generatedType = LONG
            case (FloatTag, DOUBLE) => bc.dconst(value.doubleValue);     generatedType = DOUBLE
            case (NullTag,  _     ) => bc.emit(asm.Opcodes.ACONST_NULL); generatedType = srNullRef
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
    def fieldLoad(field: Symbol, hostClass: Symbol): Unit = fieldOp(field, isLoad = true, hostClass)

    /*
     * must-single-thread
     */
    def fieldStore(field: Symbol, hostClass: Symbol): Unit = fieldOp(field, isLoad = false, hostClass)

    /*
     * must-single-thread
     */
    private def fieldOp(field: Symbol, isLoad: Boolean, hostClass: Symbol): Unit = {
      val owner      = internalName(if (hostClass == null) field.owner else hostClass)
      val fieldJName = field.javaSimpleName.toString
      val fieldDescr = symInfoTK(field).descriptor
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
          val tp = typeToBType(const.typeValue)
          // classOf[Int] is transformed to Integer.TYPE by CleanUp
          assert(!tp.isPrimitive, s"expected class type in classOf[T], found primitive type $tp")
          mnode.visitLdcInsn(tp.toASMType)

        case EnumTag   =>
          val sym       = const.symbolValue
          val ownerName = internalName(sym.owner)
          val fieldName = sym.javaSimpleName.toString
          val fieldDesc = typeToBType(sym.tpe.underlying).descriptor
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
              reporter.warning(r.pos, "Return statement found in finally-clause, discarding its return-value in favor of that of a more deeply nested return.")
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

            // TODO @lry make pattern match
            if (l.isPrimitive && r.isPrimitive)
              genConversion(l, r, cast)
            else if (l.isPrimitive) {
              bc drop l
              if (cast) {
                mnode.visitTypeInsn(asm.Opcodes.NEW, jlClassCastExceptionRef.internalName)
                bc dup ObjectRef
                emit(asm.Opcodes.ATHROW)
              } else {
                bc boolconst false
              }
            }
            else if (r.isPrimitive && cast) {
              abort(s"Erasure should have added an unboxing operation to prevent this cast. Tree: $app")
            }
            else if (r.isPrimitive) {
              bc isInstance boxedClassOfPrimitive(r.asPrimitiveBType)
            }
            else {
              assert(r.isRef, r) // ensure that it's not a method
              genCast(r.asRefBType, cast)
            }

            if (cast) r else BOOL
          } // end of genTypeApply()

          generatedType = genTypeApply()

        case Apply(fun @ Select(Super(_, _), _), args) =>
          def initModule() {
            // we initialize the MODULE$ field immediately after the super ctor
            if (!isModuleInitialized &&
              jMethodName == INSTANCE_CONSTRUCTOR_NAME &&
              fun.symbol.javaSimpleName.toString == INSTANCE_CONSTRUCTOR_NAME &&
              isStaticModuleClass(claszSymbol)) {
              isModuleInitialized = true
              mnode.visitVarInsn(asm.Opcodes.ALOAD, 0)
              mnode.visitFieldInsn(
                asm.Opcodes.PUTSTATIC,
                thisBType.internalName,
                strMODULE_INSTANCE_FIELD,
                thisBType.descriptor
              )
            }
          }
          // 'super' call: Note: since constructors are supposed to
          // return an instance of what they construct, we have to take
          // special care. On JVM they are 'void', and Scala forbids (syntactically)
          // to call super constructors explicitly and/or use their 'returned' value.
          // therefore, we can ignore this fact, and generate code that leaves nothing
          // on the stack (contrary to what the type in the AST says).
          mnode.visitVarInsn(asm.Opcodes.ALOAD, 0)
          genLoadArguments(args, paramTKs(app))
          generatedType = genCallMethod(fun.symbol, InvokeStyle.Super, app.pos)
          initModule()

        // 'new' constructor call: Note: since constructors are
        // thought to return an instance of what they construct,
        // we have to 'simulate' it by DUPlicating the freshly created
        // instance (on JVM, <init> methods return VOID).
        case Apply(fun @ Select(New(tpt), nme.CONSTRUCTOR), args) =>
          val ctor = fun.symbol
          assert(ctor.isClassConstructor, s"'new' call to non-constructor: ${ctor.name}")

          generatedType = tpeTK(tpt)
          assert(generatedType.isRef, s"Non reference type cannot be instantiated: $generatedType")

          generatedType match {
            case arr @ ArrayBType(componentType) =>
              genLoadArguments(args, paramTKs(app))
              val dims     = arr.dimension
              var elemKind = arr.elementType
              val argsSize = args.length
              if (argsSize > dims) {
                reporter.error(app.pos, s"too many arguments for array constructor: found ${args.length} but array has only $dims dimension(s)")
              }
              if (argsSize < dims) {
                /* In one step:
                 *   elemKind = new BType(BType.ARRAY, arr.off + argsSize, arr.len - argsSize)
                 * however the above does not enter a TypeName for each nested arrays in chrs.
                 */
                for (i <- args.length until dims) elemKind = ArrayBType(elemKind)
              }
              argsSize match {
                case 1 => bc newarray elemKind
                case _ => // this is currently dead code is Scalac, unlike in Dotty
                  val descr = ("[" * argsSize) + elemKind.descriptor // denotes the same as: arrayN(elemKind, argsSize).descriptor
                  mnode.visitMultiANewArrayInsn(descr, argsSize)
              }

            case rt: ClassBType =>
              assert(classBTypeFromSymbol(ctor.owner) == rt, s"Symbol ${ctor.owner.fullName} is different from $rt")
              mnode.visitTypeInsn(asm.Opcodes.NEW, rt.internalName)
              bc dup generatedType
              genLoadArguments(args, paramTKs(app))
              genCallMethod(ctor, InvokeStyle.Special, app.pos)

            case _ =>
              abort(s"Cannot instantiate $tpt of kind: $generatedType")
          }
        case Apply(fun, args) if app.hasAttachment[delambdafy.LambdaMetaFactoryCapable] =>
          val attachment = app.attachments.get[delambdafy.LambdaMetaFactoryCapable].get
          genLoadArguments(args, paramTKs(app))
          genInvokeDynamicLambda(attachment.target, attachment.arity, attachment.functionalInterface, attachment.sam, attachment.isSerializable, attachment.addScalaSerializableMarker)
          generatedType = methodBTypeFromSymbol(fun.symbol).returnType

        case Apply(fun, List(expr)) if currentRun.runDefinitions.isBox(fun.symbol) =>
          val nativeKind = tpeTK(expr)
          genLoad(expr, nativeKind)
          val MethodNameAndType(mname, methodType) = srBoxesRuntimeBoxToMethods(nativeKind)
          bc.invokestatic(srBoxesRunTimeRef.internalName, mname, methodType.descriptor, itf = false, app.pos)
          generatedType = boxResultType(fun.symbol)

        case Apply(fun, List(expr)) if currentRun.runDefinitions.isUnbox(fun.symbol) =>
          genLoad(expr)
          val boxType = unboxResultType(fun.symbol)
          generatedType = boxType
          val MethodNameAndType(mname, methodType) = srBoxesRuntimeUnboxToMethods(boxType)
          bc.invokestatic(srBoxesRunTimeRef.internalName, mname, methodType.descriptor, itf = false, app.pos)

        case app @ Apply(fun, args) =>
          val sym = fun.symbol

          if (sym.isLabel) { // jump to a label
            genLoadLabelArguments(args, labelDef(sym), app.pos)
            bc goTo programPoint(sym)
          } else if (isPrimitive(sym)) { // primitive method call
            generatedType = genPrimitiveOp(app, expectedType)
          } else { // normal method call
            def isTraitSuperAccessorBodyCall = app.hasAttachment[UseInvokeSpecial.type]
            val invokeStyle =
              if (sym.isStaticMember)
                InvokeStyle.Static
              else if (sym.isPrivate || sym.isClassConstructor) InvokeStyle.Special
              else if (isTraitSuperAccessorBodyCall)
                InvokeStyle.Special
              else InvokeStyle.Virtual

            if (invokeStyle.hasInstance) genLoadQualifier(fun)
            genLoadArguments(args, paramTKs(app))

            val Select(qual, _) = fun // fun is a Select, also checked in genLoadQualifier
            if (sym == definitions.Array_clone) {
              // Special-case Array.clone, introduced in 36ef60e. The goal is to generate this call
              // as "[I.clone" instead of "java/lang/Object.clone". This is consistent with javac.
              // Arrays have a public method `clone` (jls 10.7).
              //
              // The JVMS is not explicit about this, but that receiver type can be an array type
              // descriptor (instead of a class internal name):
              //   invokevirtual  #2; //Method "[I".clone:()Ljava/lang/Object
              //
              // Note that using `Object.clone()` would work as well, but only because the JVM
              // relaxes protected access specifically if the receiver is an array:
              //   http://hg.openjdk.java.net/jdk8/jdk8/hotspot/file/87ee5ee27509/src/share/vm/interpreter/linkResolver.cpp#l439
              // Example: `class C { override def clone(): Object = "hi" }`
              // Emitting `def f(c: C) = c.clone()` as `Object.clone()` gives a VerifyError.
              val target: String = tpeTK(qual).asRefBType.classOrArrayType
              val methodBType = methodBTypeFromSymbol(sym)
              bc.invokevirtual(target, sym.javaSimpleName.toString, methodBType.descriptor, app.pos)
              generatedType = methodBType.returnType
            } else {
              val receiverClass = if (!invokeStyle.isVirtual) null else {
                // receiverClass is used in the bytecode to as the method receiver. using sym.owner
                // may lead to IllegalAccessErrors, see 9954eaf / aladdin bug 455.
                val qualSym = qual.tpe.typeSymbol
                if (qualSym == ArrayClass) {
                  // For invocations like `Array(1).hashCode` or `.wait()`, use Object as receiver
                  // in the bytecode. Using the array descriptor (like we do for clone above) seems
                  // to work as well, but it seems safer not to change this. Javac also uses Object.
                  // Note that array apply/update/length are handled by isPrimitive (above).
                  assert(sym.owner == ObjectClass, s"unexpected array call: ${show(app)}")
                  ObjectClass
                } else qualSym
              }

              generatedType = genCallMethod(sym, invokeStyle, app.pos, receiverClass)

              // Check if the Apply tree has an InlineAnnotatedAttachment, added by the typer
              // for callsites marked `f(): @inline/noinline`. For nullary calls, the attachment
              // is on the Select node (not on the Apply node added by UnCurry).
              def recordInlineAnnotated(t: Tree): Unit = {
                if (t.hasAttachment[InlineAnnotatedAttachment]) lastInsn match {
                  case m: MethodInsnNode =>
                    if (app.hasAttachment[NoInlineCallsiteAttachment.type]) noInlineAnnotatedCallsites += m
                    else inlineAnnotatedCallsites += m
                  case _ =>
                } else t match {
                  case Apply(fun, _) => recordInlineAnnotated(fun)
                  case _ =>
                }
              }
              recordInlineAnnotated(app)
            }
          }
      }

      generatedType
    } // end of genApply()

    private def genArrayValue(av: ArrayValue): BType = {
      val ArrayValue(tpt @ TypeTree(), elems) = av

      val elmKind       = tpeTK(tpt)
      val generatedType = ArrayBType(elmKind)

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
      var switchBlocks: List[Tuple2[asm.Label, Tree]] = Nil

      // collect switch blocks and their keys, but don't emit yet any switch-block.
      for (caze @ CaseDef(pat, guard, body) <- tree.cases) {
        assert(guard == EmptyTree, guard)
        val switchBlockPoint = new asm.Label
        switchBlocks ::= ((switchBlockPoint, body))
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
        val (caseLabel, caseBody) = sb
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
        for ((sym, start) <- varsInScope.reverse) { emitLocalVarScope(sym, start, end) }
      }
      varsInScope = savedScope
    }

    def adapt(from: BType, to: BType) {
      if (!from.conformsTo(to).get) {
        to match {
          case UNIT => bc drop from
          case _    => bc.emitT2T(from, to)
        }
      } else if (from.isNothingType) {
        /* There are two possibilities for from.isNothingType: emitting a "throw e" expressions and
         * loading a (phantom) value of type Nothing.
         *
         * The Nothing type in Scala's type system does not exist in the JVM. In bytecode, Nothing
         * is mapped to scala.runtime.Nothing$. To the JVM, a call to Predef.??? looks like it would
         * return an object of type Nothing$. We need to do something with that phantom object on
         * the stack. "Phantom" because it never exists: such methods always throw, but the JVM does
         * not know that.
         *
         * Note: The two verifiers (old: type inference, new: type checking) have different
         * requirements. Very briefly:
         *
         * Old (http://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.10.2.1): at
         * each program point, no matter what branches were taken to get there
         *   - Stack is same size and has same typed values
         *   - Local and stack values need to have consistent types
         *   - In practice, the old verifier seems to ignore unreachable code and accept any
         *     instructions after an ATHROW. For example, there can be another ATHROW (without
         *     loading another throwable first).
         *
         * New (http://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.10.1)
         *   - Requires consistent stack map frames. GenBCode always generates stack frames.
         *   - In practice: the ASM library computes stack map frames for us (ClassWriter). Emitting
         *     correct frames after an ATHROW is probably complex, so ASM uses the following strategy:
         *       - Every time when generating an ATHROW, a new basic block is started.
         *       - During classfile writing, such basic blocks are found to be dead: no branches go there
         *       - Eliminating dead code would probably require complex shifts in the output byte buffer
         *       - But there's an easy solution: replace all code in the dead block with with
         *         `nop; nop; ... nop; athrow`, making sure the bytecode size stays the same
         *       - The corresponding stack frame can be easily generated: on entering a dead the block,
         *         the frame requires a single Throwable on the stack.
         *       - Since there are no branches to the dead block, the frame requirements are never violated.
         *
         * To summarize the above: it does matter what we emit after an ATHROW.
         *
         * NOW: if we end up here because we emitted a load of a (phantom) value of type Nothing$,
         * there was no ATHROW emitted. So, we have to make the verifier happy and do something
         * with that value. Since Nothing$ extends Throwable, the easiest is to just emit an ATHROW.
         *
         * If we ended up here because we generated a "throw e" expression, we know the last
         * emitted instruction was an ATHROW. As explained above, it is OK to emit a second ATHROW,
         * the verifiers will be happy.
         */
        if (lastInsn.getOpcode != asm.Opcodes.ATHROW)
          emit(asm.Opcodes.ATHROW)
      } else if (from.isNullType) {
        /* After loading an expression of type `scala.runtime.Null$`, introduce POP; ACONST_NULL.
         * This is required to pass the verifier: in Scala's type system, Null conforms to any
         * reference type. In bytecode, the type Null is represented by scala.runtime.Null$, which
         * is not a subtype of all reference types. Example:
         *
         *   def nl: Null = null // in bytecode, nl has return type scala.runtime.Null$
         *   val a: String = nl  // OK for Scala but not for the JVM, scala.runtime.Null$ does not conform to String
         *
         * In order to fix the above problem, the value returned by nl is dropped and ACONST_NULL is
         * inserted instead - after all, an expression of type scala.runtime.Null$ can only be null.
         */
        if (lastInsn.getOpcode != asm.Opcodes.ACONST_NULL) {
          bc drop from
          emit(asm.Opcodes.ACONST_NULL)
        }
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
      val module = (
        if (!tree.symbol.isPackageClass) tree.symbol
        else tree.symbol.info.packageObject match {
          case NoSymbol => abort(s"SI-5604: Cannot use package as value: $tree")
          case s        => abort(s"SI-5604: found package class where package object expected: $tree")
        }
      )
      lineNumber(tree)
      genLoadModule(module)
      symInfoTK(module)
    }

    def genLoadModule(module: Symbol) {
      def inStaticMethod = methSymbol != null && methSymbol.isStaticMember
      if (claszSymbol == module.moduleClass && jMethodName != "readResolve" && !inStaticMethod) {
        mnode.visitVarInsn(asm.Opcodes.ALOAD, 0)
      } else {
        val mbt = symInfoTK(module).asClassBType
        mnode.visitFieldInsn(
          asm.Opcodes.GETSTATIC,
          mbt.internalName /* + "$" */ ,
          strMODULE_INSTANCE_FIELD,
          mbt.descriptor // for nostalgics: typeToBType(module.tpe).descriptor
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

    def genCast(to: RefBType, cast: Boolean) {
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
          genLoad(arg, ObjectRef)
          genCallMethod(String_valueOf, InvokeStyle.Static, arg.pos)

        case concatenations =>
          bc.genStartConcat(tree.pos)
          for (elem <- concatenations) {
            val loadedElem = elem match {
              case Apply(boxOp, value :: Nil) if currentRun.runDefinitions.isBox(boxOp.symbol) =>
                // Eliminate boxing of primitive values. Boxing is introduced by erasure because
                // there's only a single synthetic `+` method "added" to the string class.
                value

              case _ => elem
            }
            val elemType = tpeTK(loadedElem)
            genLoad(loadedElem, elemType)
            bc.genConcat(elemType, loadedElem.pos)
          }
          bc.genEndConcat(tree.pos)
      }
      StringRef
    }

    /**
     * Generate a method invocation. If `specificReceiver != null`, it is used as receiver in the
     * invocation instruction, otherwise `method.owner`. A specific receiver class is needed to
     * prevent an IllegalAccessError, (aladdin bug 455).
     */
    def genCallMethod(method: Symbol, style: InvokeStyle, pos: Position, specificReceiver: Symbol = null): BType = {
      val methodOwner = method.owner
      // the class used in the invocation's method descriptor in the classfile
      val receiverClass = {
        if (specificReceiver != null)
          assert(style.isVirtual || specificReceiver == methodOwner, s"specificReceiver can only be specified for virtual calls. $method - $specificReceiver")

        val useSpecificReceiver = specificReceiver != null && !specificReceiver.isBottomClass
        val receiver = if (useSpecificReceiver) specificReceiver else methodOwner

        // workaround for a JVM bug: https://bugs.openjdk.java.net/browse/JDK-8154587
        // when an interface method overrides a member of Object (note that all interfaces implicitly
        // have superclass Object), the receiver needs to be the interface declaring the override (and
        // not a sub-interface that inherits it). example:
        //   trait T { override def clone(): Object = "" }
        //   trait U extends T
        //   class C extends U
        //   class D { def f(u: U) = u.clone() }
        // The invocation `u.clone()` needs `T` as a receiver:
        //   - using Object is illegal, as Object.clone is protected
        //   - using U results in a `NoSuchMethodError: U.clone. This is the JVM bug.
        // Note that a mixin forwarder is generated, so the correct method is executed in the end:
        //   class C { override def clone(): Object = super[T].clone() }
        val isTraitMethodOverridingObjectMember = {
          receiver != methodOwner && // fast path - the boolean is used to pick either of these two, if they are the same it does not matter
            style.isVirtual &&
            receiver.isTraitOrInterface &&
            ObjectTpe.decl(method.name).exists && // fast path - compute overrideChain on the next line only if necessary
            method.overrideChain.last.owner == ObjectClass
        }
        if (isTraitMethodOverridingObjectMember) methodOwner else receiver
      }

      receiverClass.info // ensure types the type is up to date; erasure may add lateINTERFACE to traits
      val receiverBType = classBTypeFromSymbol(receiverClass)
      val receiverName = receiverBType.internalName

      def needsInterfaceCall(sym: Symbol) = {
        sym.isTraitOrInterface ||
          sym.isJavaDefined && sym.isNonBottomSubClass(definitions.ClassfileAnnotationClass)
      }

      val jname  = method.javaSimpleName.toString
      val bmType = methodBTypeFromSymbol(method)
      val mdescr = bmType.descriptor

      val isInterface = receiverBType.isInterface.get
      import InvokeStyle._
      if (style == Super) {
        assert(receiverClass == methodOwner, s"for super call, expecting $receiverClass == $methodOwner")
        if (receiverClass.isTrait && !receiverClass.isJavaDefined) {
          val staticDesc = MethodBType(typeToBType(method.owner.info) :: bmType.argumentTypes, bmType.returnType).descriptor
          val staticName = traitSuperAccessorName(method).toString
          bc.invokestatic(receiverName, staticName, staticDesc, isInterface, pos)
        } else {
          if (receiverClass.isTraitOrInterface) {
            // An earlier check in Mixin reports an error in this case, so it doesn't reach the backend
            assert(cnode.interfaces.contains(receiverName), s"cannot invokespecial $receiverName.$jname, the interface is not a direct parent.")
          }
          bc.invokespecial(receiverName, jname, mdescr, isInterface, pos)
        }
      } else {
        val opc = style match {
          case Static => Opcodes.INVOKESTATIC
          case Special => Opcodes.INVOKESPECIAL
          case Virtual => if (isInterface) Opcodes.INVOKEINTERFACE else Opcodes.INVOKEVIRTUAL
        }
        bc.emitInvoke(opc, receiverName, jname, mdescr, isInterface, pos)
      }

      bmType.returnType
    } // end of genCallMethod()

    /* Generate the scala ## method. */
    def genScalaHash(tree: Tree, applyPos: Position): BType = {
      genLoad(tree, ObjectRef)
      genCallMethod(hashMethodSym, InvokeStyle.Static, applyPos)
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

    /* Emit code to compare the two top-most stack values using the 'op' operator. */
    private def genCJUMP(success: asm.Label, failure: asm.Label, op: TestOp, tk: BType, targetIfNoJump: asm.Label, negated: Boolean = false) {
      if (targetIfNoJump == success) genCJUMP(failure, success, op.negate, tk, targetIfNoJump, negated = !negated)
      else {
        if (tk.isIntSizedType) { // BOOL, BYTE, CHAR, SHORT, or INT
          bc.emitIF_ICMP(op, success)
        } else if (tk.isRef) { // REFERENCE(_) | ARRAY(_)
          bc.emitIF_ACMP(op, success)
        } else {
          def useCmpG = if (negated) op == TestOp.GT || op == TestOp.GE else op == TestOp.LT || op == TestOp.LE
          (tk: @unchecked) match {
            case LONG   => emit(asm.Opcodes.LCMP)
            case FLOAT  => emit(if (useCmpG) asm.Opcodes.FCMPG else asm.Opcodes.FCMPL)
            case DOUBLE => emit(if (useCmpG) asm.Opcodes.DCMPG else asm.Opcodes.DCMPL)
          }
          bc.emitIF(op, success)
        }
        if (targetIfNoJump != failure) bc goTo failure
      }
    }

    /* Emits code to compare (and consume) stack-top and zero using the 'op' operator */
    private def genCZJUMP(success: asm.Label, failure: asm.Label, op: TestOp, tk: BType, targetIfNoJump: asm.Label, negated: Boolean = false) {
      if (targetIfNoJump == success) genCZJUMP(failure, success, op.negate, tk, targetIfNoJump, negated = !negated)
      else {
        if (tk.isIntSizedType) { // BOOL, BYTE, CHAR, SHORT, or INT
          bc.emitIF(op, success)
        } else if (tk.isRef) { // REFERENCE(_) | ARRAY(_)
          op match { // references are only compared with EQ and NE
            case TestOp.EQ => bc emitIFNULL    success
            case TestOp.NE => bc emitIFNONNULL success
          }
        } else {
          def useCmpG = if (negated) op == TestOp.GT || op == TestOp.GE else op == TestOp.LT || op == TestOp.LE
          (tk: @unchecked) match {
            case LONG   =>
              emit(asm.Opcodes.LCONST_0)
              emit(asm.Opcodes.LCMP)
            case FLOAT  =>
              emit(asm.Opcodes.FCONST_0)
              emit(if (useCmpG) asm.Opcodes.FCMPG else asm.Opcodes.FCMPL)
            case DOUBLE =>
              emit(asm.Opcodes.DCONST_0)
              emit(if (useCmpG) asm.Opcodes.DCMPG else asm.Opcodes.DCMPL)
          }
          bc.emitIF(op, success)
        }
        if (targetIfNoJump != failure) bc goTo failure
      }
    }

    def testOpForPrimitive(primitiveCode: Int) = (primitiveCode: @switch) match {
      case scalaPrimitives.ID => TestOp.EQ
      case scalaPrimitives.NI => TestOp.NE
      case scalaPrimitives.EQ => TestOp.EQ
      case scalaPrimitives.NE => TestOp.NE
      case scalaPrimitives.LT => TestOp.LT
      case scalaPrimitives.LE => TestOp.LE
      case scalaPrimitives.GT => TestOp.GT
      case scalaPrimitives.GE => TestOp.GE
    }

    /** Some useful equality helpers. */
    def isNull(t: Tree) = PartialFunction.cond(t) { case Literal(Constant(null)) => true }
    def isLiteral(t: Tree) = PartialFunction.cond(t) { case Literal(_) => true }
    def isNonNullExpr(t: Tree) = isLiteral(t) || ((t.symbol ne null) && t.symbol.isModule)
    /** If l or r is constant null, returns the other ; otherwise null */
    def ifOneIsNull(l: Tree, r: Tree) = if (isNull(l)) r else if (isNull(r)) l else null

    /*
     * Generate code for conditional expressions.
     * The jump targets success/failure of the test are `then-target` and `else-target` resp.
     */
    private def genCond(tree: Tree, success: asm.Label, failure: asm.Label, targetIfNoJump: asm.Label) {

      def genComparisonOp(l: Tree, r: Tree, code: Int) {
        val op = testOpForPrimitive(code)
        val nonNullSide = if (scalaPrimitives.isReferenceEqualityOp(code)) ifOneIsNull(l, r) else null
        if (nonNullSide != null) {
          // special-case reference (in)equality test for null (null eq x, x eq null)
          genLoad(nonNullSide, ObjectRef)
          genCZJUMP(success, failure, op, ObjectRef, targetIfNoJump)
        } else {
          val tk = tpeTK(l).maxType(tpeTK(r))
          genLoad(l, tk)
          genLoad(r, tk)
          genCJUMP(success, failure, op, tk, targetIfNoJump)
        }
      }

      def loadAndTestBoolean() = {
        genLoad(tree, BOOL)
        genCZJUMP(success, failure, TestOp.NE, BOOL, targetIfNoJump)
      }

      lineNumber(tree)
      tree match {

        case Apply(fun, args) if isPrimitive(fun.symbol) =>
          import scalaPrimitives.{ ZNOT, ZAND, ZOR, EQ, getPrimitive }

          // lhs and rhs of test
          lazy val Select(lhs, _) = fun
          val rhs = if (args.isEmpty) EmptyTree else args.head // args.isEmpty only for ZNOT

          def genZandOrZor(and: Boolean) {
            // reaching "keepGoing" indicates the rhs should be evaluated too (ie not short-circuited).
            val keepGoing = new asm.Label

            if (and) genCond(lhs, keepGoing, failure, targetIfNoJump = keepGoing)
            else     genCond(lhs, success,   keepGoing, targetIfNoJump = keepGoing)

            markProgramPoint(keepGoing)
            genCond(rhs, success, failure, targetIfNoJump)
          }

          getPrimitive(fun.symbol) match {
            case ZNOT   => genCond(lhs, failure, success, targetIfNoJump)
            case ZAND   => genZandOrZor(and = true)
            case ZOR    => genZandOrZor(and = false)
            case code   =>
              if (scalaPrimitives.isUniversalEqualityOp(code) && tpeTK(lhs).isClass) {
                // rewrite `==` to null tests and `equals`. not needed for arrays (`equals` is reference equality).
                if (code == EQ) genEqEqPrimitive(lhs, rhs, success, failure, targetIfNoJump, tree.pos)
                else            genEqEqPrimitive(lhs, rhs, failure, success, targetIfNoJump, tree.pos)
              } else if (scalaPrimitives.isComparisonOp(code)) {
                genComparisonOp(lhs, rhs, code)
              } else
                loadAndTestBoolean()
          }

        case _ => loadAndTestBoolean()
      }

    } // end of genCond()

    /*
     * Generate the "==" code for object references. It is equivalent of
     * if (l eq null) r eq null else l.equals(r);
     *
     * @param l       left-hand-side  of the '=='
     * @param r       right-hand-side of the '=='
     */
    def genEqEqPrimitive(l: Tree, r: Tree, success: asm.Label, failure: asm.Label, targetIfNoJump: asm.Label, pos: Position) {

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
        val equalsMethod: Symbol = {
          if (l.tpe <:< BoxedNumberClass.tpe) {
            if (r.tpe <:< BoxedNumberClass.tpe) platform.externalEqualsNumNum
            else if (r.tpe <:< BoxedCharacterClass.tpe) platform.externalEqualsNumChar
            else platform.externalEqualsNumObject
          } else platform.externalEquals
        }
        genLoad(l, ObjectRef)
        genLoad(r, ObjectRef)
        genCallMethod(equalsMethod, InvokeStyle.Static, pos)
        genCZJUMP(success, failure, TestOp.NE, BOOL, targetIfNoJump)
      } else {
        if (isNull(l)) {
          // null == expr -> expr eq null
          genLoad(r, ObjectRef)
          genCZJUMP(success, failure, TestOp.EQ, ObjectRef, targetIfNoJump)
        } else if (isNull(r)) {
          // expr == null -> expr eq null
          genLoad(l, ObjectRef)
          genCZJUMP(success, failure, TestOp.EQ, ObjectRef, targetIfNoJump)
        } else if (isNonNullExpr(l)) {
          // SI-7852 Avoid null check if L is statically non-null.
          genLoad(l, ObjectRef)
          genLoad(r, ObjectRef)
          genCallMethod(Object_equals, InvokeStyle.Virtual, pos)
          genCZJUMP(success, failure, TestOp.NE, BOOL, targetIfNoJump)
        } else {
          // l == r -> if (l eq null) r eq null else l.equals(r)
          val eqEqTempLocal = locals.makeLocal(ObjectRef, nme.EQEQ_LOCAL_VAR.toString)
          val lNull    = new asm.Label
          val lNonNull = new asm.Label

          genLoad(l, ObjectRef)
          genLoad(r, ObjectRef)
          locals.store(eqEqTempLocal)
          bc dup ObjectRef
          genCZJUMP(lNull, lNonNull, TestOp.EQ, ObjectRef, targetIfNoJump = lNull)

          markProgramPoint(lNull)
          bc drop ObjectRef
          locals.load(eqEqTempLocal)
          genCZJUMP(success, failure, TestOp.EQ, ObjectRef, targetIfNoJump = lNonNull)

          markProgramPoint(lNonNull)
          locals.load(eqEqTempLocal)
          genCallMethod(Object_equals, InvokeStyle.Virtual, pos)
          genCZJUMP(success, failure, TestOp.NE, BOOL, targetIfNoJump)
        }
      }
    }


    def genSynchronized(tree: Apply, expectedType: BType): BType
    def genLoadTry(tree: Try): BType

    def genInvokeDynamicLambda(lambdaTarget: Symbol, arity: Int, functionalInterface: Symbol, sam: Symbol, isSerializable: Boolean, addScalaSerializableMarker: Boolean) {
      val isStaticMethod = lambdaTarget.hasFlag(Flags.STATIC)
      def asmType(sym: Symbol) = classBTypeFromSymbol(sym).toASMType

      val isInterface = lambdaTarget.owner.isTrait
      val implMethodHandle =
        new asm.Handle(if (lambdaTarget.hasFlag(Flags.STATIC)) asm.Opcodes.H_INVOKESTATIC else if (isInterface) asm.Opcodes.H_INVOKEINTERFACE else asm.Opcodes.H_INVOKEVIRTUAL,
          classBTypeFromSymbol(lambdaTarget.owner).internalName,
          lambdaTarget.name.toString,
          methodBTypeFromSymbol(lambdaTarget).descriptor,
          /* itf = */ isInterface)
      val receiver = if (isStaticMethod) Nil else lambdaTarget.owner :: Nil
      val (capturedParams, lambdaParams) = lambdaTarget.paramss.head.splitAt(lambdaTarget.paramss.head.length - arity)
      val invokedType = asm.Type.getMethodDescriptor(asmType(functionalInterface), (receiver ::: capturedParams).map(sym => typeToBType(sym.info).toASMType): _*)
      val constrainedType = new MethodBType(lambdaParams.map(p => typeToBType(p.tpe)), typeToBType(lambdaTarget.tpe.resultType)).toASMType
      val samMethodType = methodBTypeFromSymbol(sam).toASMType
      val markers = if (addScalaSerializableMarker) classBTypeFromSymbol(definitions.SerializableClass).toASMType :: Nil else Nil
      visitInvokeDynamicInsnLMF(bc.jmethod, sam.name.toString, invokedType, samMethodType, implMethodHandle, constrainedType, isSerializable, markers)
      if (isSerializable)
        addIndyLambdaImplMethod(cnode.name, implMethodHandle :: Nil)
    }
  }

  private def visitInvokeDynamicInsnLMF(jmethod: MethodNode, samName: String, invokedType: String, samMethodType: asm.Type,
                                        implMethodHandle: asm.Handle, instantiatedMethodType: asm.Type,
                                        serializable: Boolean, markerInterfaces: Seq[asm.Type]) = {
    import java.lang.invoke.LambdaMetafactory.{FLAG_MARKERS, FLAG_SERIALIZABLE}
    def flagIf(b: Boolean, flag: Int): Int = if (b) flag else 0
    val flags = FLAG_MARKERS | flagIf(serializable, FLAG_SERIALIZABLE)
    val bsmArgs = Seq(samMethodType, implMethodHandle, instantiatedMethodType, Int.box(flags), Int.box(markerInterfaces.length)) ++ markerInterfaces
    jmethod.visitInvokeDynamicInsn(samName, invokedType, lambdaMetaFactoryAltMetafactoryHandle, bsmArgs: _*)
  }

}
