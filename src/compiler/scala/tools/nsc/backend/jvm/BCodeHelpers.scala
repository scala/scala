/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala
package tools.nsc
package backend.jvm

import scala.tools.asm
import scala.collection.mutable
import scala.tools.nsc.io.AbstractFile
import GenBCode._
import BackendReporting._

/*
 *  Traits encapsulating functionality to convert Scala AST Trees into ASM ClassNodes.
 *
 *  @author  Miguel Garcia, http://lamp.epfl.ch/~magarcia/ScalaCompilerCornerReloaded
 *  @version 1.0
 *
 */
abstract class BCodeHelpers extends BCodeIdiomatic with BytecodeWriters {
  import global._
  import bTypes._
  import coreBTypes._

  /*
   * must-single-thread
   */
  def getFileForClassfile(base: AbstractFile, clsName: String, suffix: String): AbstractFile = {
    getFile(base, clsName, suffix)
  }

  /*
   * must-single-thread
   */
  def getOutFolder(csym: Symbol, cName: String, cunit: CompilationUnit): _root_.scala.tools.nsc.io.AbstractFile =
    _root_.scala.util.Try {
      outputDirectory(csym)
    }.recover {
      case ex: Throwable =>
        reporter.error(cunit.body.pos, s"Couldn't create file for class $cName\n${ex.getMessage}")
        null
    }.get

  var pickledBytes = 0 // statistics

  // -----------------------------------------------------------------------------------------
  // finding the least upper bound in agreement with the bytecode verifier (given two internal names handed by ASM)
  // Background:
  //  http://gallium.inria.fr/~xleroy/publi/bytecode-verification-JAR.pdf
  //  http://comments.gmane.org/gmane.comp.java.vm.languages/2293
  //  https://issues.scala-lang.org/browse/SI-3872
  // -----------------------------------------------------------------------------------------

  /*  An `asm.ClassWriter` that uses `jvmWiseLUB()`
   *  The internal name of the least common ancestor of the types given by inameA and inameB.
   *  It's what ASM needs to know in order to compute stack map frames, http://asm.ow2.org/doc/developer-guide.html#controlflow
   */
  final class CClassWriter(flags: Int) extends asm.ClassWriter(flags) {

    /**
     * This method is thread-safe: it depends only on the BTypes component, which does not depend
     * on global. TODO @lry move to a different place where no global is in scope, on bTypes.
     */
    override def getCommonSuperClass(inameA: String, inameB: String): String = {
      val a = classBTypeFromInternalName(inameA)
      val b = classBTypeFromInternalName(inameB)
      val lub = a.jvmWiseLUB(b).get
      val lubName = lub.internalName
      assert(lubName != "scala/Any")
      lubName // ASM caches the answer during the lifetime of a ClassWriter. We outlive that. Not sure whether caching on our side would improve things.
    }
  }

  /*
   * must-single-thread
   */
  object isJavaEntryPoint {

    /*
     * must-single-thread
     */
    def apply(sym: Symbol, csymCompUnit: CompilationUnit): Boolean = {
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

  /*
   * must-single-thread
   */
  def initBytecodeWriter(entryPoints: List[Symbol]): BytecodeWriter = {
    settings.outputDirs.getSingleOutput match {
      case Some(f) if f hasExtension "jar" =>
        // If no main class was specified, see if there's only one
        // entry point among the classes going into the jar.
        if (settings.mainClass.isDefault) {
          entryPoints map (_.fullName('.')) match {
            case Nil      =>
              log("No Main-Class designated or discovered.")
            case name :: Nil =>
              log(s"Unique entry point: setting Main-Class to $name")
              settings.mainClass.value = name
            case names =>
              log(s"No Main-Class due to multiple entry points:\n  ${names.mkString("\n  ")}")
          }
        }
        else log(s"Main-Class was specified: ${settings.mainClass.value}")

        new DirectToJarfileWriter(f.file)

      case _ => factoryNonJarBytecodeWriter()
    }
  }

  /*
   *  must-single-thread
   */
  def fieldSymbols(cls: Symbol): List[Symbol] = {
    for (f <- cls.info.decls.toList ;
         if !f.isMethod && f.isTerm && !f.isModule
    ) yield f
  }

  /*
   * can-multi-thread
   */
  def methodSymbols(cd: ClassDef): List[Symbol] = {
    cd.impl.body collect { case dd: DefDef => dd.symbol }
  }

  /*
   *  must-single-thread
   */
  def serialVUID(csym: Symbol): Option[Long] = csym getAnnotation definitions.SerialVersionUIDAttr collect {
    case AnnotationInfo(_, _, (_, LiteralAnnotArg(const)) :: Nil) => const.longValue
  }

  /*
   * Populates the InnerClasses JVM attribute with `refedInnerClasses`.
   * In addition to inner classes mentioned somewhere in `jclass` (where `jclass` is a class file being emitted)
   * `refedInnerClasses` should contain those inner classes defined as direct member classes of `jclass`
   * but otherwise not mentioned in `jclass`.
   *
   * `refedInnerClasses` may contain duplicates,
   * need not contain the enclosing inner classes of each inner class it lists (those are looked up for consistency).
   *
   * This method serializes in the InnerClasses JVM attribute in an appropriate order,
   * not necessarily that given by `refedInnerClasses`.
   *
   * can-multi-thread
   */
  final def addInnerClassesASM(jclass: asm.ClassVisitor, refedInnerClasses: List[ClassBType]) {
    val allNestedClasses = refedInnerClasses.flatMap(_.enclosingNestedClassesChain.get).distinct

    // sorting ensures nested classes are listed after their enclosing class thus satisfying the Eclipse Java compiler
    for (nestedClass <- allNestedClasses.sortBy(_.internalName.toString)) {
      // Extract the innerClassEntry - we know it exists, enclosingNestedClassesChain only returns nested classes.
      val Some(e) = nestedClass.innerClassAttributeEntry.get
      jclass.visitInnerClass(e.name, e.outerName, e.innerName, e.flags)
    }
  }

  /*
   * Custom attribute (JVMS 4.7.1) "ScalaSig" used as marker only
   * i.e., the pickle is contained in a custom annotation, see:
   *   (1) `addAnnotations()`,
   *   (2) SID # 10 (draft) - Storage of pickled Scala signatures in class files, http://www.scala-lang.org/sid/10
   *   (3) SID # 5 - Internals of Scala Annotations, http://www.scala-lang.org/sid/5
   * That annotation in turn is not related to the "java-generic-signature" (JVMS 4.7.9)
   * other than both ending up encoded as attributes (JVMS 4.7)
   * (with the caveat that the "ScalaSig" attribute is associated to some classes,
   * while the "Signature" attribute can be associated to classes, methods, and fields.)
   *
   */
  trait BCPickles {

    import scala.reflect.internal.pickling.{ PickleFormat, PickleBuffer }

    val versionPickle = {
      val vp = new PickleBuffer(new Array[Byte](16), -1, 0)
      assert(vp.writeIndex == 0, vp)
      vp writeNat PickleFormat.MajorVersion
      vp writeNat PickleFormat.MinorVersion
      vp writeNat 0
      vp
    }

    /*
     * can-multi-thread
     */
    def createJAttribute(name: String, b: Array[Byte], offset: Int, len: Int): asm.Attribute = {
      val dest = new Array[Byte](len)
      System.arraycopy(b, offset, dest, 0, len)
      new asm.CustomAttr(name, dest)
    }

    /*
     * can-multi-thread
     */
    def pickleMarkerLocal = {
      createJAttribute(tpnme.ScalaSignatureATTR.toString, versionPickle.bytes, 0, versionPickle.writeIndex)
    }

    /*
     * can-multi-thread
     */
    def pickleMarkerForeign = {
      createJAttribute(tpnme.ScalaATTR.toString, new Array[Byte](0), 0, 0)
    }

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

  } // end of trait BCPickles

  trait BCInnerClassGen {

    def debugLevel = settings.debuginfo.indexOfChoice

    final val emitSource = debugLevel >= 1
    final val emitLines  = debugLevel >= 2
    final val emitVars   = debugLevel >= 3

    /*
     *  Contains class-symbols that:
     *    (a) are known to denote inner classes
     *    (b) are mentioned somewhere in the class being generated.
     *
     *  In other words, the lifetime of `innerClassBufferASM` is associated to "the class being generated".
     */
    final val innerClassBufferASM = mutable.Set.empty[ClassBType]

    /**
     * The class internal name for a given class symbol. If the symbol describes a nested class, the
     * ClassBType is added to the innerClassBufferASM.
     */
    final def internalName(sym: Symbol): String = {
      // For each java class, the scala compiler creates a class and a module (thus a module class).
      // If the `sym` is a java module class, we use the java class instead. This ensures that we
      // register the class (instead of the module class) in innerClassBufferASM.
      // The two symbols have the same name, so the resulting internalName is the same.
      // Phase travel (exitingPickler) required for SI-6613 - linkedCoC is only reliable in early phases (nesting)
      val classSym = if (sym.isJavaDefined && sym.isModuleClass) exitingPickler(sym.linkedClassOfClass) else sym
      getClassBTypeAndRegisterInnerClass(classSym).internalName
    }

    /**
     * The ClassBType for a class symbol. If the class is nested, the ClassBType is added to the
     * innerClassBufferASM.
     *
     * TODO: clean up the way we track referenced inner classes.
     * doing it during code generation is not correct when the optimizer changes the code.
     */
    final def getClassBTypeAndRegisterInnerClass(sym: Symbol): ClassBType = {
      val r = classBTypeFromSymbol(sym)
      if (r.isNestedClass.get) innerClassBufferASM += r
      r
    }

    /**
     * The BType for a type reference. If the result is a ClassBType for a nested class, it is added
     * to the innerClassBufferASM.
     * TODO: clean up the way we track referenced inner classes.
     */
    final def toTypeKind(t: Type): BType = typeToBType(t) match {
      case c: ClassBType if c.isNestedClass.get =>
        innerClassBufferASM += c
        c
      case r => r
    }

    /**
     * Class components that are nested classes are added to the innerClassBufferASM.
     * TODO: clean up the way we track referenced inner classes.
     */
    final def asmMethodType(msym: Symbol): MethodBType = {
      val r = methodBTypeFromSymbol(msym)
      (r.returnType :: r.argumentTypes) foreach {
        case c: ClassBType if c.isNestedClass.get => innerClassBufferASM += c
        case _ =>
      }
      r
    }

    /**
     * The jvm descriptor of a type. If `t` references a nested class, its ClassBType is added to
     * the innerClassBufferASM.
     */
    final def descriptor(t: Type):   String = { toTypeKind(t).descriptor   }

    /**
     * The jvm descriptor for a symbol. If `sym` represents a nested class, its ClassBType is added
     * to the innerClassBufferASM.
     */
    final def descriptor(sym: Symbol): String = { getClassBTypeAndRegisterInnerClass(sym).descriptor }

  } // end of trait BCInnerClassGen

  trait BCAnnotGen extends BCInnerClassGen {

    import genASM.{ubytesToCharArray, arrEncode}
    import bCodeAsmCommon.{shouldEmitAnnotation, isRuntimeVisible}

    /*
     *  can-multi-thread
     */
    private def strEncode(sb: ScalaSigBytes): String = {
      val ca = ubytesToCharArray(sb.sevenBitsMayBeZero)
      new java.lang.String(ca)
      // debug val bvA = new asm.ByteVector; bvA.putUTF8(s)
      // debug val enc: Array[Byte] = scala.reflect.internal.pickling.ByteCodecs.encode(bytes)
      // debug assert(enc(idx) == bvA.getByte(idx + 2))
      // debug assert(bvA.getLength == enc.size + 2)
    }

    /*
     * For arg a LiteralAnnotArg(constt) with const.tag in {ClazzTag, EnumTag}
     * as well as for arg a NestedAnnotArg
     *   must-single-thread
     * Otherwise it's safe to call from multiple threads.
     */
    def emitArgument(av:   asm.AnnotationVisitor,
                     name: String,
                     arg:  ClassfileAnnotArg) {
      (arg: @unchecked) match {

        case LiteralAnnotArg(const) =>
          if (const.isNonUnitAnyVal) { av.visit(name, const.value) }
          else {
            const.tag match {
              case StringTag  =>
                assert(const.value != null, const) // TODO this invariant isn't documented in `case class Constant`
                av.visit(name, const.stringValue)  // `stringValue` special-cases null, but that execution path isn't exercised for a const with StringTag
              case ClazzTag   => av.visit(name, toTypeKind(const.typeValue).toASMType)
              case EnumTag =>
                val edesc  = descriptor(const.tpe) // the class descriptor of the enumeration class.
                val evalue = const.symbolValue.name.toString // value the actual enumeration value.
                av.visitEnum(name, edesc, evalue)
            }
          }

        case sb @ ScalaSigBytes(bytes) =>
          // see http://www.scala-lang.org/sid/10 (Storage of pickled Scala signatures in class files)
          // also JVMS Sec. 4.7.16.1 The element_value structure and JVMS Sec. 4.4.7 The CONSTANT_Utf8_info Structure.
          if (sb.fitsInOneString) {
            av.visit(name, strEncode(sb))
          } else {
            val arrAnnotV: asm.AnnotationVisitor = av.visitArray(name)
            for(arg <- genASM.arrEncode(sb)) { arrAnnotV.visit(name, arg) }
            arrAnnotV.visitEnd()
          }          // for the lazy val in ScalaSigBytes to be GC'ed, the invoker of emitAnnotations() should hold the ScalaSigBytes in a method-local var that doesn't escape.

        case ArrayAnnotArg(args) =>
          val arrAnnotV: asm.AnnotationVisitor = av.visitArray(name)
          for(arg <- args) { emitArgument(arrAnnotV, null, arg) }
          arrAnnotV.visitEnd()

        case NestedAnnotArg(annInfo) =>
          val AnnotationInfo(typ, args, assocs) = annInfo
          assert(args.isEmpty, args)
          val desc = descriptor(typ) // the class descriptor of the nested annotation class
          val nestedVisitor = av.visitAnnotation(name, desc)
          emitAssocs(nestedVisitor, assocs)
      }
    }

    /*
     * In general,
     *   must-single-thread
     * but not  necessarily always.
     */
    def emitAssocs(av: asm.AnnotationVisitor, assocs: List[(Name, ClassfileAnnotArg)]) {
      for ((name, value) <- assocs) {
        emitArgument(av, name.toString(), value)
      }
      av.visitEnd()
    }

    /*
     * must-single-thread
     */
    def emitAnnotations(cw: asm.ClassVisitor, annotations: List[AnnotationInfo]) {
      for(annot <- annotations; if shouldEmitAnnotation(annot)) {
        val AnnotationInfo(typ, args, assocs) = annot
        assert(args.isEmpty, args)
        val av = cw.visitAnnotation(descriptor(typ), isRuntimeVisible(annot))
        emitAssocs(av, assocs)
      }
    }

    /*
     * must-single-thread
     */
    def emitAnnotations(mw: asm.MethodVisitor, annotations: List[AnnotationInfo]) {
      for(annot <- annotations; if shouldEmitAnnotation(annot)) {
        val AnnotationInfo(typ, args, assocs) = annot
        assert(args.isEmpty, args)
        val av = mw.visitAnnotation(descriptor(typ), isRuntimeVisible(annot))
        emitAssocs(av, assocs)
      }
    }

    /*
     * must-single-thread
     */
    def emitAnnotations(fw: asm.FieldVisitor, annotations: List[AnnotationInfo]) {
      for(annot <- annotations; if shouldEmitAnnotation(annot)) {
        val AnnotationInfo(typ, args, assocs) = annot
        assert(args.isEmpty, args)
        val av = fw.visitAnnotation(descriptor(typ), isRuntimeVisible(annot))
        emitAssocs(av, assocs)
      }
    }

    /*
     * must-single-thread
     */
    def emitParamAnnotations(jmethod: asm.MethodVisitor, pannotss: List[List[AnnotationInfo]]) {
      val annotationss = pannotss map (_ filter shouldEmitAnnotation)
      if (annotationss forall (_.isEmpty)) return
      for ((annots, idx) <- annotationss.zipWithIndex;
           annot <- annots) {
        val AnnotationInfo(typ, args, assocs) = annot
        assert(args.isEmpty, args)
        val pannVisitor: asm.AnnotationVisitor = jmethod.visitParameterAnnotation(idx, descriptor(typ), isRuntimeVisible(annot))
        emitAssocs(pannVisitor, assocs)
      }
    }

  } // end of trait BCAnnotGen

  trait BCJGenSigGen {

    def getCurrentCUnit(): CompilationUnit

    /* @return
     *   - `null` if no Java signature is to be added (`null` is what ASM expects in these cases).
     *   - otherwise the signature in question
     *
     * must-single-thread
     */
    def getGenericSignature(sym: Symbol, owner: Symbol): String = genASM.getGenericSignature(sym, owner, getCurrentCUnit())

  } // end of trait BCJGenSigGen

  trait BCForwardersGen extends BCAnnotGen with BCJGenSigGen {

    /* Adds a @remote annotation, actual use unknown.
     *
     * Invoked from genMethod() and addForwarder().
     *
     * must-single-thread
     */
    def addRemoteExceptionAnnot(isRemoteClass: Boolean, isJMethodPublic: Boolean, meth: Symbol) {
      val needsAnnotation = (
        (  isRemoteClass ||
           isRemote(meth) && isJMethodPublic
        ) && !(meth.throwsAnnotations contains definitions.RemoteExceptionClass)
      )
      if (needsAnnotation) {
        val c   = Constant(definitions.RemoteExceptionClass.tpe)
        val arg = Literal(c) setType c.tpe
        meth.addAnnotation(appliedType(definitions.ThrowsClass, c.tpe), arg)
      }
    }

    /* Add a forwarder for method m. Used only from addForwarders().
     *
     * must-single-thread
     */
    private def addForwarder(isRemoteClass: Boolean, jclass: asm.ClassVisitor, module: Symbol, m: Symbol) {
      val moduleName     = internalName(module)
      val methodInfo     = module.thisType.memberInfo(m)
      val paramJavaTypes: List[BType] = methodInfo.paramTypes map toTypeKind
      // val paramNames     = 0 until paramJavaTypes.length map ("x_" + _)

      /* Forwarders must not be marked final,
       *  as the JVM will not allow redefinition of a final static method,
       *  and we don't know what classes might be subclassing the companion class.  See SI-4827.
       */
      // TODO: evaluate the other flags we might be dropping on the floor here.
      // TODO: ACC_SYNTHETIC ?
      val flags = GenBCode.PublicStatic | (
        if (m.isVarargsMethod) asm.Opcodes.ACC_VARARGS else 0
      )

      // TODO needed? for(ann <- m.annotations) { ann.symbol.initialize }
      val jgensig = genASM.staticForwarderGenericSignature(m, module, getCurrentCUnit())
      addRemoteExceptionAnnot(isRemoteClass, hasPublicBitSet(flags), m)
      val (throws, others) = m.annotations partition (_.symbol == definitions.ThrowsClass)
      val thrownExceptions: List[String] = getExceptions(throws)

      val jReturnType = toTypeKind(methodInfo.resultType)
      val mdesc = MethodBType(paramJavaTypes, jReturnType).descriptor
      val mirrorMethodName = m.javaSimpleName.toString
      val mirrorMethod: asm.MethodVisitor = jclass.visitMethod(
        flags,
        mirrorMethodName,
        mdesc,
        jgensig,
        mkArray(thrownExceptions)
      )

      emitAnnotations(mirrorMethod, others)
      emitParamAnnotations(mirrorMethod, m.info.params.map(_.annotations))

      mirrorMethod.visitCode()

      mirrorMethod.visitFieldInsn(asm.Opcodes.GETSTATIC, moduleName, strMODULE_INSTANCE_FIELD, descriptor(module))

      var index = 0
      for(jparamType <- paramJavaTypes) {
        mirrorMethod.visitVarInsn(jparamType.typedOpcode(asm.Opcodes.ILOAD), index)
        assert(!jparamType.isInstanceOf[MethodBType], jparamType)
        index += jparamType.size
      }

      mirrorMethod.visitMethodInsn(asm.Opcodes.INVOKEVIRTUAL, moduleName, mirrorMethodName, asmMethodType(m).descriptor, false)
      mirrorMethod.visitInsn(jReturnType.typedOpcode(asm.Opcodes.IRETURN))

      mirrorMethod.visitMaxs(0, 0) // just to follow protocol, dummy arguments
      mirrorMethod.visitEnd()

    }

    /* Add forwarders for all methods defined in `module` that don't conflict
     *  with methods in the companion class of `module`. A conflict arises when
     *  a method with the same name is defined both in a class and its companion object:
     *  method signature is not taken into account.
     *
     * must-single-thread
     */
    def addForwarders(isRemoteClass: Boolean, jclass: asm.ClassVisitor, jclassName: String, moduleClass: Symbol) {
      assert(moduleClass.isModuleClass, moduleClass)
      debuglog(s"Dumping mirror class for object: $moduleClass")

      val linkedClass  = moduleClass.companionClass
      lazy val conflictingNames: Set[Name] = {
        (linkedClass.info.members collect { case sym if sym.name.isTermName => sym.name }).toSet
      }
      debuglog(s"Potentially conflicting names for forwarders: $conflictingNames")

      for (m <- moduleClass.info.membersBasedOnFlags(bCodeAsmCommon.ExcludedForwarderFlags, symtab.Flags.METHOD)) {
        if (m.isType || m.isDeferred || (m.owner eq definitions.ObjectClass) || m.isConstructor)
          debuglog(s"No forwarder for '$m' from $jclassName to '$moduleClass'")
        else if (conflictingNames(m.name))
          log(s"No forwarder for $m due to conflict with ${linkedClass.info.member(m.name)}")
        else if (m.hasAccessBoundary)
          log(s"No forwarder for non-public member $m")
        else {
          log(s"Adding static forwarder for '$m' from $jclassName to '$moduleClass'")
          addForwarder(isRemoteClass, jclass, moduleClass, m)
        }
      }
    }

    /*
     * Quoting from JVMS 4.7.5 The Exceptions Attribute
     *   "The Exceptions attribute indicates which checked exceptions a method may throw.
     *    There may be at most one Exceptions attribute in each method_info structure."
     *
     * The contents of that attribute are determined by the `String[] exceptions` argument to ASM's ClassVisitor.visitMethod()
     * This method returns such list of internal names.
     *
     * must-single-thread
     */
    def getExceptions(excs: List[AnnotationInfo]): List[String] = {
      for (ThrownException(exc) <- excs.distinct)
      yield internalName(exc)
    }

  } // end of trait BCForwardersGen

  trait BCClassGen extends BCInnerClassGen {

    // Used as threshold above which a tableswitch bytecode instruction is preferred over a lookupswitch.
    // There's a space tradeoff between these multi-branch instructions (details in the JVM spec).
    // The particular value in use for `MIN_SWITCH_DENSITY` reflects a heuristic.
    val MIN_SWITCH_DENSITY = 0.7

    /*
     *  Add public static final field serialVersionUID with value `id`
     *
     *  can-multi-thread
     */
    def addSerialVUID(id: Long, jclass: asm.ClassVisitor) {
      // add static serialVersionUID field if `clasz` annotated with `@SerialVersionUID(uid: Long)`
      jclass.visitField(
        GenBCode.PublicStaticFinal,
        "serialVersionUID",
        "J",
        null, // no java-generic-signature
        new java.lang.Long(id)
      ).visitEnd()
    }

    /**
     * Add:
     * private static java.util.Map $deserializeLambdaCache$ = null
     * private static Object $deserializeLambda$(SerializedLambda l) {
     *   var cache = $deserializeLambdaCache$
     *   if (cache eq null) {
     *     cache = new java.util.HashMap()
     *     $deserializeLambdaCache$ = cache
     *   }
     *   return scala.compat.java8.runtime.LambdaDeserializer.deserializeLambda(MethodHandles.lookup(), cache, l);
     * }
     */
    def addLambdaDeserialize(clazz: Symbol, jclass: asm.ClassVisitor): Unit = {
      val cw = jclass
      import scala.tools.asm.Opcodes._

      // Need to force creation of BTypes for these as `getCommonSuperClass` is called on
      // automatically computing the max stack size (`visitMaxs`) during method writing.
      javaUtilHashMapReference
      javaUtilMapReference

      cw.visitInnerClass("java/lang/invoke/MethodHandles$Lookup", "java/lang/invoke/MethodHandles", "Lookup", ACC_PUBLIC + ACC_FINAL + ACC_STATIC)

      {
        val fv = cw.visitField(ACC_PRIVATE + ACC_STATIC + ACC_SYNTHETIC, "$deserializeLambdaCache$", "Ljava/util/Map;", null, null)
        fv.visitEnd()
      }

      {
        val mv = cw.visitMethod(ACC_PRIVATE + ACC_STATIC + ACC_SYNTHETIC, "$deserializeLambda$", "(Ljava/lang/invoke/SerializedLambda;)Ljava/lang/Object;", null, null)
        mv.visitCode()
        // javaBinaryName returns the internal name of a class. Also used in BTypesFromsymbols.classBTypeFromSymbol.
        mv.visitFieldInsn(GETSTATIC, clazz.javaBinaryName.toString, "$deserializeLambdaCache$", "Ljava/util/Map;")
        mv.visitVarInsn(ASTORE, 1)
        mv.visitVarInsn(ALOAD, 1)
        val l0 = new asm.Label()
        mv.visitJumpInsn(IFNONNULL, l0)
        mv.visitTypeInsn(NEW, "java/util/HashMap")
        mv.visitInsn(DUP)
        mv.visitMethodInsn(INVOKESPECIAL, "java/util/HashMap", "<init>", "()V", false)
        mv.visitVarInsn(ASTORE, 1)
        mv.visitVarInsn(ALOAD, 1)
        mv.visitFieldInsn(PUTSTATIC, clazz.javaBinaryName.toString, "$deserializeLambdaCache$", "Ljava/util/Map;")
        mv.visitLabel(l0)
        mv.visitFieldInsn(GETSTATIC, "scala/compat/java8/runtime/LambdaDeserializer$", "MODULE$", "Lscala/compat/java8/runtime/LambdaDeserializer$;")
        mv.visitMethodInsn(INVOKESTATIC, "java/lang/invoke/MethodHandles", "lookup", "()Ljava/lang/invoke/MethodHandles$Lookup;", false)
        mv.visitVarInsn(ALOAD, 1)
        mv.visitVarInsn(ALOAD, 0)
        mv.visitMethodInsn(INVOKEVIRTUAL, "scala/compat/java8/runtime/LambdaDeserializer$", "deserializeLambda", "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/util/Map;Ljava/lang/invoke/SerializedLambda;)Ljava/lang/Object;", false)
        mv.visitInsn(ARETURN)
        mv.visitEnd()
      }
    }
  } // end of trait BCClassGen

  /* functionality for building plain and mirror classes */
  abstract class JCommonBuilder
    extends BCInnerClassGen
    with    BCAnnotGen
    with    BCForwardersGen
    with    BCPickles { }

  /* builder of mirror classes */
  class JMirrorBuilder extends JCommonBuilder {

    private var cunit: CompilationUnit = _
    def getCurrentCUnit(): CompilationUnit = cunit;

    /* Generate a mirror class for a top-level module. A mirror class is a class
     *  containing only static methods that forward to the corresponding method
     *  on the MODULE instance of the given Scala object.  It will only be
     *  generated if there is no companion class: if there is, an attempt will
     *  instead be made to add the forwarder methods to the companion class.
     *
     *  must-single-thread
     */
    def genMirrorClass(moduleClass: Symbol, cunit: CompilationUnit): asm.tree.ClassNode = {
      assert(moduleClass.isModuleClass)
      assert(moduleClass.companionClass == NoSymbol, moduleClass)
      innerClassBufferASM.clear()
      this.cunit = cunit

      val bType = mirrorClassClassBType(moduleClass)
      val mirrorClass = new asm.tree.ClassNode
      mirrorClass.visit(
        classfileVersion,
        bType.info.get.flags,
        bType.internalName,
        null /* no java-generic-signature */,
        ObjectReference.internalName,
        EMPTY_STRING_ARRAY
      )

      if (emitSource)
        mirrorClass.visitSource("" + cunit.source, null /* SourceDebugExtension */)

      val ssa = getAnnotPickle(bType.internalName, moduleClass.companionSymbol)
      mirrorClass.visitAttribute(if (ssa.isDefined) pickleMarkerLocal else pickleMarkerForeign)
      emitAnnotations(mirrorClass, moduleClass.annotations ++ ssa)

      addForwarders(isRemote(moduleClass), mirrorClass, bType.internalName, moduleClass)

      innerClassBufferASM ++= bType.info.get.nestedClasses
      addInnerClassesASM(mirrorClass, innerClassBufferASM.toList)

      mirrorClass.visitEnd()

      ("" + moduleClass.name) // this side-effect is necessary, really.

      mirrorClass
    }

  } // end of class JMirrorBuilder

  /* builder of bean info classes */
  class JBeanInfoBuilder extends BCInnerClassGen {

    /*
     * Generate a bean info class that describes the given class.
     *
     * @author Ross Judson (ross.judson@soletta.com)
     *
     * must-single-thread
     */
    def genBeanInfoClass(cls: Symbol, cunit: CompilationUnit, fieldSymbols: List[Symbol], methodSymbols: List[Symbol]): asm.tree.ClassNode = {

      def javaSimpleName(s: Symbol): String = { s.javaSimpleName.toString }

      innerClassBufferASM.clear()

      val flags = javaFlags(cls)

      val beanInfoName  = (internalName(cls) + "BeanInfo")
      val beanInfoClass = new asm.tree.ClassNode
      beanInfoClass.visit(
        classfileVersion,
        flags,
        beanInfoName,
        null, // no java-generic-signature
        "scala/beans/ScalaBeanInfo",
        EMPTY_STRING_ARRAY
      )

      beanInfoClass.visitSource(
        cunit.source.toString,
        null /* SourceDebugExtension */
      )

      var fieldList = List[String]()

      for (f <- fieldSymbols if f.hasGetter;
                 g = f.getterIn(cls);
                 s = f.setterIn(cls);
	         if g.isPublic && !(f.name startsWith "$")
          ) {
             // inserting $outer breaks the bean
             fieldList = javaSimpleName(f) :: javaSimpleName(g) :: (if (s != NoSymbol) javaSimpleName(s) else null) :: fieldList
      }

      val methodList: List[String] =
	     for (m <- methodSymbols
	          if !m.isConstructor &&
	          m.isPublic &&
	          !(m.name startsWith "$") &&
	          !m.isGetter &&
	          !m.isSetter)
       yield javaSimpleName(m)

      val constructor = beanInfoClass.visitMethod(
        asm.Opcodes.ACC_PUBLIC,
        INSTANCE_CONSTRUCTOR_NAME,
        "()V",
        null, // no java-generic-signature
        EMPTY_STRING_ARRAY // no throwable exceptions
      )

      val stringArrayJType: BType = ArrayBType(StringReference)
      val conJType: BType = MethodBType(
        classBTypeFromSymbol(definitions.ClassClass) :: stringArrayJType :: stringArrayJType :: Nil,
        UNIT
      )

      def push(lst: List[String]) {
        var fi = 0
        for (f <- lst) {
          constructor.visitInsn(asm.Opcodes.DUP)
          constructor.visitLdcInsn(new java.lang.Integer(fi))
          if (f == null) { constructor.visitInsn(asm.Opcodes.ACONST_NULL) }
          else           { constructor.visitLdcInsn(f) }
          constructor.visitInsn(StringReference.typedOpcode(asm.Opcodes.IASTORE))
          fi += 1
        }
      }

      constructor.visitCode()

      constructor.visitVarInsn(asm.Opcodes.ALOAD, 0)
      // push the class
      constructor.visitLdcInsn(classBTypeFromSymbol(cls).toASMType)

      // push the string array of field information
      constructor.visitLdcInsn(new java.lang.Integer(fieldList.length))
      constructor.visitTypeInsn(asm.Opcodes.ANEWARRAY, StringReference.internalName)
      push(fieldList)

      // push the string array of method information
      constructor.visitLdcInsn(new java.lang.Integer(methodList.length))
      constructor.visitTypeInsn(asm.Opcodes.ANEWARRAY, StringReference.internalName)
      push(methodList)

      // invoke the superclass constructor, which will do the
      // necessary java reflection and create Method objects.
      constructor.visitMethodInsn(asm.Opcodes.INVOKESPECIAL, "scala/beans/ScalaBeanInfo", INSTANCE_CONSTRUCTOR_NAME, conJType.descriptor, false)
      constructor.visitInsn(asm.Opcodes.RETURN)

      constructor.visitMaxs(0, 0) // just to follow protocol, dummy arguments
      constructor.visitEnd()

      innerClassBufferASM ++= classBTypeFromSymbol(cls).info.get.nestedClasses
      addInnerClassesASM(beanInfoClass, innerClassBufferASM.toList)

      beanInfoClass.visitEnd()

      beanInfoClass
    }

  } // end of class JBeanInfoBuilder

  trait JAndroidBuilder {
    self: BCInnerClassGen =>

    /* From the reference documentation of the Android SDK:
     *  The `Parcelable` interface identifies classes whose instances can be written to and restored from a `Parcel`.
     *  Classes implementing the `Parcelable` interface must also have a static field called `CREATOR`,
     *  which is an object implementing the `Parcelable.Creator` interface.
     */
    val androidFieldName = newTermName("CREATOR")

    /*
     * must-single-thread
     */
    def isAndroidParcelableClass(sym: Symbol) =
      (AndroidParcelableInterface != NoSymbol) &&
      (sym.parentSymbols contains AndroidParcelableInterface)

    /*
     * must-single-thread
     */
    def legacyAddCreatorCode(clinit: asm.MethodVisitor, cnode: asm.tree.ClassNode, thisName: String) {
      // this tracks the inner class in innerClassBufferASM, if needed.
      val androidCreatorType = getClassBTypeAndRegisterInnerClass(AndroidCreatorClass)
      val tdesc_creator = androidCreatorType.descriptor

      cnode.visitField(
        GenBCode.PublicStaticFinal,
        "CREATOR",
        tdesc_creator,
        null, // no java-generic-signature
        null  // no initial value
      ).visitEnd()

      val moduleName = (thisName + "$")

      // GETSTATIC `moduleName`.MODULE$ : `moduleName`;
      clinit.visitFieldInsn(
        asm.Opcodes.GETSTATIC,
        moduleName,
        strMODULE_INSTANCE_FIELD,
        "L" + moduleName + ";"
      )

      // INVOKEVIRTUAL `moduleName`.CREATOR() : android.os.Parcelable$Creator;
      val bt = MethodBType(Nil, androidCreatorType)
      clinit.visitMethodInsn(
        asm.Opcodes.INVOKEVIRTUAL,
        moduleName,
        "CREATOR",
        bt.descriptor,
        false
      )

      // PUTSTATIC `thisName`.CREATOR;
      clinit.visitFieldInsn(
        asm.Opcodes.PUTSTATIC,
        thisName,
        "CREATOR",
        tdesc_creator
      )
    }

  } // end of trait JAndroidBuilder
}
