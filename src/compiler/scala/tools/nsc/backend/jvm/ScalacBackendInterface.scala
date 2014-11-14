package scala.tools.nsc.backend.jvm

import scala.tools.nsc.Global
import scala.tools.asm
import scala.reflect.io.AbstractFile



abstract class ScalacBackendInterface[G <: Global](val global: G) extends BackendInterface{
  import global._
  import definitions._
  type Symbol = global.Symbol
  type Type = global.Type
  type Annotation = global.AnnotationInfo
  type Flags = Long
  type Tree = global.Tree
  type CompilationUnit = global.CompilationUnit
  type Constant = global.Constant
  type Literal = global.Literal
  type Position = global.Position
  type Name = global.Name
  type LabelDef = global.LabelDef
  type ClassDef = global.ClassDef

  val NoSymbol = global.NoSymbol

  import scala.tools.nsc.symtab._

  // TODO @lry avoiding going through through missingHook for every line in the REPL: https://github.com/scala/scala/commit/8d962ed4ddd310cc784121c426a2e3f56a112540
  lazy val AndroidParcelableInterface : Symbol = getClassIfDefined("android.os.Parcelable")
  lazy val AndroidCreatorClass        : Symbol = getClassIfDefined("android.os.Parcelable$Creator")

  val BeanInfoAttr: Symbol = requiredClass[scala.beans.BeanInfo]

  /* The Object => String overload. */
  lazy val String_valueOf: Symbol = {
    getMember(StringModule, nme.valueOf) filter (sym => sym.info.paramTypes match {
      case List(pt) => pt.typeSymbol == ObjectClass
      case _        => false
    })
  }


  def debuglevel: Int = settings.debuginfo.indexOfChoice
  def settings_debug: Boolean = settings.debug

  /**
   * The member classes of a class symbol. Note that the result of this method depends on the
   * current phase, for example, after lambdalift, all local classes become member of the enclosing
   * class.
   */
  def memberClassesOf(classSymbol: Symbol): List[Symbol] = classSymbol.info.decls.collect({
    case sym if sym.isClass =>
      sym
    case sym if sym.isModule =>
      val r = exitingPickler(sym.moduleClass)
      assert(r != NoSymbol, sym.fullLocationString)
      r
  })(collection.breakOut)

  def shouldEmitAnnotation(annot: Annotation): Boolean = {
    annot.symbol.initialize.isJavaDefined &&
      annot.matches(ClassfileAnnotationClass) &&
      retentionPolicyOf(annot) != AnnotationRetentionPolicySourceValue &&
      annot.args.isEmpty
  }

  def isRuntimeVisible(annot: Annotation): Boolean = {
    annot.atp.typeSymbol.getAnnotation(AnnotationRetentionAttr) match {
      case Some(retentionAnnot) =>
        retentionAnnot.assocs.contains(nme.value -> LiteralAnnotArg(new Constant(AnnotationRetentionPolicyRuntimeValue)))
      case _ =>
        // SI-8926: if the annotation class symbol doesn't have a @RetentionPolicy annotation, the
        // annotation is emitted with visibility `RUNTIME`
        true
    }
  }

  private def retentionPolicyOf(annot: AnnotationInfo): Symbol =
    annot.atp.typeSymbol.getAnnotation(AnnotationRetentionAttr).map(_.assocs).map(assoc =>
      assoc.collectFirst {
        case (`nme`.value, LiteralAnnotArg(Constant(value: Symbol))) => value
      }).flatten.getOrElse(AnnotationRetentionPolicyClassValue)

  lazy val AnnotationRetentionPolicyModule       = AnnotationRetentionPolicyAttr.companionModule
  lazy val AnnotationRetentionPolicySourceValue  = AnnotationRetentionPolicyModule.tpe.member(TermName("SOURCE"))
  lazy val AnnotationRetentionPolicyClassValue   = AnnotationRetentionPolicyModule.tpe.member(TermName("CLASS"))
  lazy val AnnotationRetentionPolicyRuntimeValue = AnnotationRetentionPolicyModule.tpe.member(TermName("RUNTIME"))

  def informProgress(msg: String): Unit = if (settings.verbose) inform("[" + msg + "]")

  def ExcludedForwarderFlags: Flags = {
    import scala.tools.nsc.symtab.Flags._
    // Should include DEFERRED but this breaks findMember.
    SPECIALIZED | LIFTED | PROTECTED | STATIC | EXPANDEDNAME | BridgeAndPrivateFlags | MACRO
  }



  def isNull(t: Tree): Boolean = t match {
    case Literal(Constant(null)) => true
    case _ => false
  }
  def isLiteral(t: Tree): Boolean = t match {
    case Literal(_) => true
    case _ => false
  }
  def isNonNullExpr(t: Tree): Boolean = isLiteral(t) || ((t.symbol ne null) && t.symbol.isModule)
  def ifOneIsNull(l: Tree, r: Tree): Tree = if (isNull(l)) r else if (isNull(r)) l else null

  private val primitiveCompilationUnits = Set(
    "Unit.scala",
    "Boolean.scala",
    "Char.scala",
    "Byte.scala",
    "Short.scala",
    "Int.scala",
    "Float.scala",
    "Long.scala",
    "Double.scala"
  )

  /**
   * True if the current compilation unit is of a primitive class (scala.Boolean et al).
   * Used only in assertions.
   */
  def isCompilingPrimitive = {
    primitiveCompilationUnits(currentUnit.source.file.name)
  }

  def isCompilingArray = {
    currentUnit.source.file.name == "Array.scala"
  }

  val MODULE_INSTANCE_FIELD = nme.MODULE_INSTANCE_FIELD.toString
  def internalNameString(offset: Int, length: Int) = new String(global.chrs, offset, length)


  override def emitAsmp = if (settings.Ygenasmp.isSetByUser) Some(settings.Ygenasmp.value) else None

  override def dumpClasses: Option[String] = if(settings.Ydumpclasses.isSetByUser) Some(settings.Ydumpclasses.value) else None

  override def mainClass = {
    if (settings.mainClass.isDefault) None
    else Some(settings.mainClass.value)
  }


  override def boxMethods = currentRun.runDefinitions.boxMethod

  // (class, method)
  override def unboxMethods = currentRun.runDefinitions.boxMethod

  val hashMethodSym: Symbol = getMember(ScalaRunTimeModule, nme.hash_)

  abstract class ScalacSymbolHelper(sym: Symbol) extends SymbolHelper{
     def nestedClasses: List[Symbol] = exitingPhase(currentRun.lambdaliftPhase)(memberClassesOf(sym))


    def isGetClass: Boolean = definitions.isGetClass(sym)

    def addRemoteRemoteExceptionAnnotation: Unit = {
      val c   = new Constant(RemoteExceptionClass.tpe)
      val arg = new Literal(c) setType c.tpe
      sym.addAnnotation(appliedType(definitions.ThrowsClass, c.tpe), arg)
    }

    def linkedClass: Symbol = exitingPickler(sym.linkedClassOfClass) // linkedCoC does not work properly in late phases
     def companionModuleMembers: List[Symbol] = {
      // phase travel to exitingPickler: this makes sure that memberClassesOf only sees member classes,
      // not local classes of the companion module (E in the exmaple) that were lifted by lambdalift.
      if (linkedClass.isTopLevelModuleClass) exitingPickler(memberClassesOf(linkedClass))
      else Nil
    }

    /**
     * All interfaces implemented by a class, except for those inherited through the superclass.
     *
     */
     def superInterfaces: List[Symbol] =  {

      // Additional interface parents based on annotations and other cues
      def newParentForAnnotation(ann: AnnotationInfo): Symbol =
        if (ann.symbol eq RemoteAttr) RemoteInterfaceClass
        else NoSymbol

      val superInterfaces0: List[Symbol] = sym.mixinClasses
      val superInterfaces = existingSymbols(superInterfaces0 ++ sym.annotations.map(newParentForAnnotation)).distinct

      assert(!superInterfaces.contains(NoSymbol), s"found NoSymbol among: ${superInterfaces.mkString(", ")}")
      assert(superInterfaces.forall(s => s.isInterface || s.isTrait), s"found non-interface among: ${superInterfaces.mkString(", ")}")

      erasure.minimizeInterfaces(superInterfaces.map(_.info)).map(_.typeSymbol)
    }
    /**
     * True for module classes of package level objects. The backend will generate a mirror class for
     * such objects.
     */
    def isTopLevelModuleClass: Boolean = exitingPickler {
      // phase travel to pickler required for isNestedClass (looks at owner)
      val r = sym.isModuleClass && !sym.isNestedClass
      // The mixin phase adds the `lateMODULE` flag to trait implementation classes. Since the flag
      // is late, it should not be visible here inside the time travel. We check this.
      if (r) assert(!sym.isImplClass, s"isModuleClass should be false for impl class $sym")
      r
    }

    /**
     * True for module classes of modules that are top-level or owned only by objects. Module classes
     * for such objects will get a MODULE$ flag and a corresponding static initializer.
     */
    def isStaticModuleClass: Boolean = {
      /* (1) Phase travel to to pickler is required to exclude implementation classes; they have the
       * lateMODULEs after mixin, so isModuleClass would be true.
       * (2) isStaticModuleClass is a source-level property. See comment on isOriginallyStaticOwner.
       */
      exitingPickler { // (1)
        sym.isModuleClass &&
          sym.originalOwner.isOriginallyStaticOwner // (2)
      }
    }

    /**
     * This is basically a re-implementation of sym.isStaticOwner, but using the originalOwner chain.
     *
     * The problem is that we are interested in a source-level property. Various phases changed the
     * symbol's properties in the meantime, mostly lambdalift modified (destructively) the owner.
     * Therefore, `sym.isStatic` is not what we want. For example, in
     *   object T { def f { object U } }
     * the owner of U is T, so UModuleClass.isStatic is true. Phase travel does not help here.
     */
    def isOriginallyStaticOwner: Boolean = {
      sym.isPackageClass || sym.isModuleClass && sym.isOriginallyStaticOwner
    }

     def isSynchronized: Boolean = sym.hasFlag(Flags.SYNCHRONIZED)

     def enclosingClassSym: Symbol =  {
      if (sym.isJavaDefined && sym.rawowner.isModuleClass) {
        // Example java source: class C { static class D { } }
        // The Scala compiler creates a class and a module symbol for C. Because D is a static
        // nested class, the symbol for D is nested in the module class C (not in the class C).
        // For the InnerClass attribute, we use the class symbol C, which represents the situation
        // in the source code.

        // Cannot use innerClassSym.isStatic: this method looks at the owner, which is a package
        // at this pahse (after lambdalift, flatten).
        assert(sym.originalOwner.isOriginallyStaticOwner, sym.originalOwner)

        // phase travel for linkedCoC - does not always work in late phases
        exitingPickler(sym.rawowner.linkedClassOfClass)
      }
      else sym.rawowner
    }

    def fieldSymbols: List[Symbol] = {
      for (f <- sym.info.decls.toList ;
           if !f.isMethod && f.isTerm && !f.isModule
      ) yield f
    }


    def methodSymbols: List[Symbol] = {
      // cd.impl.body collect { case dd: DefDef => dd.symbol }
      for (f <- sym.info.decls.toList ;
           if f.isMethod
      ) yield f
    }


    def shouldEmitForwarders: Boolean = {
      exitingPickler { !(sym.name.toString contains '$') && sym.hasModuleFlag && !sym.isImplClass && !sym.isNestedClass }
    }

    def isPrivate: Boolean = {
      // constructors of module classes should be private. introduced in b06edbc, probably to prevent
      // creating module instances from java. for nested modules, the constructor needs to be public
      // since they are created by the outer class and stored in a field. a java client can create
      // new instances via outerClassInstance.new InnerModuleClass$().
      // TODO: do this early, mark the symbol private.
      sym.isPrivate || (sym.isPrimaryConstructor && sym.owner.isTopLevelModuleClass)
    }

    def outputDirectory(sym: Symbol): AbstractFile =
      settings.outputDirs outputDirFor enteringFlatten(sym.sourceFile)

     def isFinal: Boolean = {
      // Symbols marked in source as `final` have the FINAL flag. (In the past, the flag was also
      // added to modules and module classes, not anymore since 296b706).
      // Note that the presence of the `FINAL` flag on a symbol does not correspond 1:1 to emitting
      // ACC_FINAL in bytecode.
      //
      // Top-level modules are marked ACC_FINAL in bytecode (even without the FINAL flag). Nested
      // objects don't get the flag to allow overriding (under -Yoverride-objects, SI-5676).
      //
      // For fields, only eager val fields can receive ACC_FINAL. vars or lazy vals can't:
      // Source: http://docs.oracle.com/javase/specs/jls/se7/html/jls-17.html#jls-17.5.3
      // "Another problem is that the specification allows aggressive
      // optimization of final fields. Within a thread, it is permissible to
      // reorder reads of a final field with those modifications of a final
      // field that do not take place in the constructor."
      //
      // A var or lazy val which is marked final still has meaning to the
      // scala compiler. The word final is heavily overloaded unfortunately;
      // for us it means "not overridable". At present you can't override
      // vars regardless; this may change.
      //
      // The logic does not check .isFinal (which checks flags for the FINAL flag,
      // and includes symbols marked lateFINAL) instead inspecting rawflags so
      // we can exclude lateFINAL. Such symbols are eligible for inlining, but to
      // avoid breaking proxy software which depends on subclassing, we do not
      // emit ACC_FINAL.
      (
        (((sym.rawflags & Flags.FINAL) != 0) || sym.isTopLevelModuleClass)
          && !sym.enclClass.isInterface
          && !sym.isClassConstructor
          && !sym.isMutable // lazy vals and vars both
        )
    }

    def serialVUID(csym: Symbol): Option[Long] = csym getAnnotation definitions.SerialVersionUIDAttr collect {
      case AnnotationInfo(_, _, (_, LiteralAnnotArg(const)) :: Nil) => const.longValue
    }

    def isJavaEntryPoint: Boolean = {
      def fail(msg: String, pos: Position = sym.pos) = {
        reporter.warning(sym.pos,
          sym.name +
            s" has a main method with parameter type Array[String], but ${sym.fullName('.')} will not be a runnable program.\n  Reason: $msg"
          // TODO: make this next claim true, if possible
          //   by generating valid main methods as static in module classes
          //   not sure what the jvm allows here
          // + "  You can still run the program by calling it as " + sym.javaSimpleName + " instead."
        )
        false
      }
      def failNoForwarder(msg: String) = {
        fail(s"$msg, which means no static forwarder can be generated.\n")
      }
      val possibles = if (sym.hasModuleFlag) (sym.tpe nonPrivateMember nme.main).alternatives else Nil
      val hasApproximate = possibles exists { m =>
        m.info match {
          case MethodType(p :: Nil, _) => p.tpe.typeSymbol == definitions.ArrayClass
          case _                       => false
        }
      }
      // At this point it's a module with a main-looking method, so either succeed or warn that it isn't.
      hasApproximate && {
        // Before erasure so we can identify generic mains.
        enteringErasure {
          val companion     = sym.linkedClassOfClass

          if (definitions.hasJavaMainMethod(companion))
            failNoForwarder("companion contains its own main method")
          else if (companion.tpe.member(nme.main) != NoSymbol)
          // this is only because forwarders aren't smart enough yet
            failNoForwarder("companion contains its own main method (implementation restriction: no main is allowed, regardless of signature)")
          else if (companion.isTrait)
            failNoForwarder("companion is a trait")
          // Now either succeeed, or issue some additional warnings for things which look like
          // attempts to be java main methods.
          else (possibles exists definitions.isJavaMainMethod) || {
            possibles exists { m =>
              m.info match {
                case PolyType(_, _) =>
                  fail("main methods cannot be generic.")
                case MethodType(params, res) =>
                  if (res.typeSymbol :: params exists (_.isAbstractType))
                    fail("main methods cannot refer to type parameters or abstract types.", m.pos)
                  else
                    definitions.isJavaMainMethod(m) || fail("main method must have exact signature (Array[String])Unit", m.pos)
                case tp =>
                  fail(s"don't know what this is: $tp", m.pos)
              }
            }
          }
        }
      }
    }
  }

  def log(msg: => String): Unit = global synchronized { global.log(msg) }

  def sourceFileFor(cu: CompilationUnit): String = cu.source.toString

  def noForwarders: Boolean = settings.noForwarders

  var pickledBytes = 0

  /*  Returns a ScalaSignature annotation if it must be added to this class, none otherwise.
 *  This annotation must be added to the class' annotations list when generating them.
 *
 *  Depending on whether the returned option is defined, it adds to `jclass` one of:
 *    (a) the ScalaSig marker attribute
 *        (indicating that a scala-signature-annotation aka pickle is present in this class); or
 *    (b) the Scala marker attribute
 *        (indicating that a scala-signature-annotation aka pickle is to be found in another file).
 *
 *
 *  @param jclassName The class file that is being readied.
 *  @param sym    The symbol for which the signature has been entered in the symData map.
 *                This is different than the symbol
 *                that is being generated in the case of a mirror class.
 *  @return       An option that is:
 *                - defined and contains an AnnotationInfo of the ScalaSignature type,
 *                  instantiated with the pickle signature for sym.
 *                - empty if the jclass/sym pair must not contain a pickle.
 *
 *  must-single-thread
 */
  def getAnnotPickle(jclassName: String, sym: Symbol): Option[AnnotationInfo] = {
    currentRun.symData get sym match {
      case Some(pickle) if !nme.isModuleName(newTermName(jclassName)) =>
        val scalaAnnot = {
          val sigBytes = ScalaSigBytes(pickle.bytes.take(pickle.writeIndex))
          AnnotationInfo(sigBytes.sigAnnot, Nil, (nme.bytes, sigBytes) :: Nil)
        }
        pickledBytes += pickle.writeIndex
        currentRun.symData -= sym
        currentRun.symData -= sym.companionSymbol
        Some(scalaAnnot)
      case _ =>
        None
    }
  }

  abstract class ScalaCTypeHelper(t: Type) extends TypeHelper {
    /**
     * This method returns the BType for a type reference, for example a parameter type.
     *
     * If the result is a ClassBType for a nested class, it is added to the innerClassBufferASM.
     *
     * If `t` references a class, toTypeKind ensures that the class is not an implementation class.
     * See also comment on getClassBTypeAndRegisterInnerClass, which is invoked for implementation
     * classes.
     */
    final def toTypeKind(ctx: BCodeHelpers)(storage: ctx.BCInnerClassGen): ctx.bTypes.BType = {
      import ctx.bTypes._
      import coreBTypes._
      /**
       * Primitive types are represented as TypeRefs to the class symbol of, for example, scala.Int.
       * The `primitiveTypeMap` maps those class symbols to the corresponding PrimitiveBType.
       */
      def primitiveOrClassToBType(sym: Symbol): BType = {
        assert(sym.isClass, sym)
        assert(sym != ArrayClass || isCompilingArray, sym)
        primitiveTypeMap.getOrElse(sym, storage.getClassBTypeAndRegisterInnerClass(sym.asInstanceOf[ctx.int.Symbol]))
      }

      /**
       * When compiling Array.scala, the type parameter T is not erased and shows up in method
       * signatures, e.g. `def apply(i: Int): T`. A TyperRef to T is replaced by ObjectReference.
       */
      def nonClassTypeRefToBType(sym: Symbol): ClassBType = {
        assert(sym.isType && isCompilingArray, sym)
        ObjectReference
      }

      t.dealiasWiden match {
        case TypeRef(_, ArrayClass, List(arg))  => ArrayBType(arg.toTypeKind(ctx)(storage))  // Array type such as Array[Int] (kept by erasure)
        case TypeRef(_, sym, _) if !sym.isClass => nonClassTypeRefToBType(sym)  // See comment on nonClassTypeRefToBType
        case TypeRef(_, sym, _)                 => primitiveOrClassToBType(sym) // Common reference to a type such as scala.Int or java.lang.String
        case ClassInfoType(_, _, sym)           => primitiveOrClassToBType(sym) // We get here, for example, for genLoadModule, which invokes toTypeKind(moduleClassSymbol.info)

        /* AnnotatedType should (probably) be eliminated by erasure. However we know it happens for
         * meta-annotated annotations (@(ann @getter) val x = 0), so we don't emit a warning.
         * The type in the AnnotationInfo is an AnnotatedTpe. Tested in jvm/annotations.scala.
         */
        case a @ AnnotatedType(_, t) =>
          debuglog(s"typeKind of annotated type $a")
          t.toTypeKind(ctx)(storage)

        /* ExistentialType should (probably) be eliminated by erasure. We know they get here for
         * classOf constants:
         *   class C[T]
         *   class T { final val k = classOf[C[_]] }
         */
        case e @ ExistentialType(_, t) =>
          debuglog(s"typeKind of existential type $e")
          t.toTypeKind(ctx)(storage)

        /* The cases below should probably never occur. They are kept for now to avoid introducing
         * new compiler crashes, but we added a warning. The compiler / library bootstrap and the
         * test suite don't produce any warning.
         */

        case tp =>
          currentUnit.warning(tp.typeSymbol.pos,
            s"an unexpected type representation reached the compiler backend while compiling $currentUnit: $tp. " +
              "If possible, please file a bug on issues.scala-lang.org.")

          tp match {
            case ThisType(ArrayClass)               => ObjectReference // was introduced in 9b17332f11 to fix SI-999, but this code is not reached in its test, or any other test
            case ThisType(sym)                      => storage.getClassBTypeAndRegisterInnerClass(sym.asInstanceOf[ctx.int.Symbol])
            case SingleType(_, sym)                 => primitiveOrClassToBType(sym)
            case ConstantType(_)                    => t.underlying.toTypeKind(ctx)(storage)
            case RefinedType(parents, _)            => parents.map(_.toTypeKind(ctx)(storage).asClassBType).reduceLeft((a, b) => a.jvmWiseLUB(b))
          }
      }
    }
  }

  def isQualifierSafeToElide(qual: Tree): Boolean = treeInfo isQualifierSafeToElide qual

  def isBox(sym: Symbol): Boolean = currentRun.runDefinitions.isBox(sym)

  def isUnbox(sym: Symbol): Boolean = currentRun.runDefinitions.isUnbox(sym)

  /*
     * For arg a LiteralAnnotArg(constt) with const.tag in {ClazzTag, EnumTag}
     * as well as for arg a NestedAnnotArg
     *   must-single-thread
     * Otherwise it's safe to call from multiple threads.
     */
  private def emitArgument(av:   asm.AnnotationVisitor,
                           name: String,
                           arg:  ClassfileAnnotArg, bcodeStore: BCodeHelpers)(innerClasesStore: bcodeStore.BCInnerClassGen) {
    (arg: @unchecked) match {

      case LiteralAnnotArg(const) =>
        if (const.isNonUnitAnyVal) { av.visit(name, const.value) }
        else {
          const.tag match {
            case StringTag  =>
              assert(const.value != null, const) // TODO this invariant isn't documented in `case class Constant`
              av.visit(name, const.stringValue)  // `stringValue` special-cases null, but that execution path isn't exercised for a const with StringTag
            case ClazzTag   => av.visit(name, const.typeValue.toTypeKind(bcodeStore)(innerClasesStore).toASMType)
            case EnumTag =>
              val edesc  = innerClasesStore.typeDescriptor(const.tpe.asInstanceOf[bcodeStore.int.Type]) // the class descriptor of the enumeration class.
              val evalue = const.symbolValue.name.toString // value the actual enumeration value.
              av.visitEnum(name, edesc, evalue)
          }
        }

      case sb @ ScalaSigBytes(bytes) =>
        // see http://www.scala-lang.org/sid/10 (Storage of pickled Scala signatures in class files)
        // also JVMS Sec. 4.7.16.1 The element_value structure and JVMS Sec. 4.4.7 The CONSTANT_Utf8_info Structure.
        if (sb.fitsInOneString) {
          av.visit(name, BCodeAsmCommon.strEncode(sb))
        } else {
          val arrAnnotV: asm.AnnotationVisitor = av.visitArray(name)
          for(arg <- BCodeAsmCommon.arrEncode(sb)) { arrAnnotV.visit(name, arg) }
          arrAnnotV.visitEnd()
        }          // for the lazy val in ScalaSigBytes to be GC'ed, the invoker of emitAnnotations() should hold the ScalaSigBytes in a method-local var that doesn't escape.

      case ArrayAnnotArg(args) =>
        val arrAnnotV: asm.AnnotationVisitor = av.visitArray(name)
        for(arg <- args) { emitArgument(arrAnnotV, null, arg, bcodeStore)(innerClasesStore) }
        arrAnnotV.visitEnd()

      case NestedAnnotArg(annInfo) =>
        val AnnotationInfo(typ, args, assocs) = annInfo
        assert(args.isEmpty, args)
        val desc = innerClasesStore.typeDescriptor(typ.asInstanceOf[bcodeStore.int.Type]) // the class descriptor of the nested annotation class
      val nestedVisitor = av.visitAnnotation(name, desc)
        emitAssocs(nestedVisitor, assocs, bcodeStore)(innerClasesStore)
    }
  }

  /*
   * In general,
   *   must-single-thread
   * but not  necessarily always.
   */
  private def emitAssocs(av: asm.AnnotationVisitor, assocs: List[(Name, ClassfileAnnotArg)], bcodeStore: BCodeHelpers)(innerClasesStore: bcodeStore.BCInnerClassGen) {
    for ((name, value) <- assocs) {
      emitArgument(av, name.toString(), value, bcodeStore)(innerClasesStore)
    }
    av.visitEnd()
  }

  /*
   * must-single-thread
   */
  def emitAnnotations(cw: asm.ClassVisitor, annotations: List[Annotation], bcodeStore: BCodeHelpers)(innerClasesStore: bcodeStore.BCInnerClassGen) {
    for(annot <- annotations; if shouldEmitAnnotation(annot)) {
      val AnnotationInfo(typ, args, assocs) = annot
      assert(args.isEmpty, args)
      val av = cw.visitAnnotation(innerClasesStore.typeDescriptor(typ.asInstanceOf[bcodeStore.int.Type]), isRuntimeVisible(annot))
      emitAssocs(av, assocs, bcodeStore)(innerClasesStore)
    }
  }

  /*
   * must-single-thread
   */
  def emitAnnotations(mw: asm.MethodVisitor, annotations: List[AnnotationInfo], bcodeStore: BCodeHelpers)(innerClasesStore: bcodeStore.BCInnerClassGen) {
    for(annot <- annotations; if shouldEmitAnnotation(annot)) {
      val AnnotationInfo(typ, args, assocs) = annot
      assert(args.isEmpty, args)
      val av = mw.visitAnnotation(innerClasesStore.typeDescriptor(typ.asInstanceOf[bcodeStore.int.Type]), isRuntimeVisible(annot))
      emitAssocs(av, assocs, bcodeStore)(innerClasesStore)
    }
  }

  /*
   * must-single-thread
   */
  def emitAnnotations(fw: asm.FieldVisitor, annotations: List[AnnotationInfo], bcodeStore: BCodeHelpers)(innerClasesStore: bcodeStore.BCInnerClassGen) {
    for(annot <- annotations; if shouldEmitAnnotation(annot)) {
      val AnnotationInfo(typ, args, assocs) = annot
      assert(args.isEmpty, args)
      val av = fw.visitAnnotation(innerClasesStore.typeDescriptor(typ.asInstanceOf[bcodeStore.int.Type]), isRuntimeVisible(annot))
      emitAssocs(av, assocs, bcodeStore)(innerClasesStore)
    }
  }

  /*
   * must-single-thread
   */
  def emitParamAnnotations(jmethod: asm.MethodVisitor, pannotss: List[List[AnnotationInfo]], bcodeStore: BCodeHelpers)(innerClasesStore: bcodeStore.BCInnerClassGen) {
    val annotationss = pannotss map (_ filter shouldEmitAnnotation)
    if (annotationss forall (_.isEmpty)) return
    for ((annots, idx) <- annotationss.zipWithIndex;
         annot <- annots) {
      val AnnotationInfo(typ, args, assocs) = annot
      assert(args.isEmpty, args)
      val pannVisitor: asm.AnnotationVisitor = jmethod.visitParameterAnnotation(idx, innerClasesStore.typeDescriptor(typ.asInstanceOf[bcodeStore.int.Type]), isRuntimeVisible(annot))
      emitAssocs(pannVisitor, assocs, bcodeStore)(innerClasesStore)
    }
  }

  def getGenericSignature(sym: Symbol, owner: Symbol): String = genASM.getGenericSignature(sym, owner)

  def getStaticForwarderGenericSignature(sym: Symbol, moduleClass: Symbol): String = genASM.staticForwarderGenericSignature(sym, moduleClass)

  def getLabelDefOwners(t: Tree): Map[Tree, List[LabelDef]] = {

    class LabelDefsFinder extends Traverser {
      val result = collection.mutable.Map.empty[Tree, List[LabelDef]]
      var acc: List[LabelDef] = Nil

      /*
       * can-multi-thread
       */
      override def traverse(tree: Tree) {
        val saved = acc
        acc = Nil
        super.traverse(tree)
        // acc contains all LabelDefs found under (but not at) `tree`
        tree match {
          case lblDf: LabelDef => acc ::= lblDf
          case _               => ()
        }
        if (acc.isEmpty) {
          acc = saved
        } else {
          result += (tree -> acc)
          acc = acc ::: saved
        }
      }
    }
    val ldf = new LabelDefsFinder()
    ldf.traverse(t)
    ldf.result.toMap
  }
}
