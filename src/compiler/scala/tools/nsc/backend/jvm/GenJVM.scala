/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Iulian Dragos
 */

// $Id$

package scala.tools.nsc.backend.jvm

import java.io.File
import java.nio.ByteBuffer

import scala.collection.immutable.{Set, ListSet}
import scala.collection.mutable.{Map, HashMap, HashSet}
import scala.tools.nsc.symtab._
import scala.tools.nsc.util.Position

import ch.epfl.lamp.fjbg._

/** This class ...
 *
 *  @author  Iulian Dragos
 *  @version 1.0
 */
abstract class GenJVM extends SubComponent {
  import global._
  import icodes._
  import icodes.opcodes._

  val phaseName = "jvm"

  /** Create a new phase */
  override def newPhase(p: Phase) = new JvmPhase(p)

  /** JVM code generation phase
   */
  class JvmPhase(prev: Phase) extends StdPhase(prev) {

    override def erasedTypes = true
    object codeGenerator extends BytecodeGenerator

    override def run: Unit = {
      if (settings.debug.value) inform("[running phase " + name + " on icode]")
      classes.values foreach codeGenerator.genClass
    }

    override def apply(unit: CompilationUnit): Unit =
      abort("JVM works on icode classes, not on compilation units!")
  }

  var pickledBytes = 0 // statistics

  /**
   * Java bytecode generator.
   *
   */
  class BytecodeGenerator {
    val MIN_SWITCH_DENSITY = 0.7
    val JAVA_LANG_STRINGBUFFER = "java.lang.StringBuffer"
    val BOXED_NUMBER = "scala.runtime.BoxedNumber"

    val stringBufferType = new JObjectType(JAVA_LANG_STRINGBUFFER)
    val toStringType = new JMethodType(JObjectType.JAVA_LANG_STRING, JType.EMPTY_ARRAY)

    def attributeType(name: String) =
      atPhase(currentRun.typerPhase)(definitions.getClass(name).tpe)

    // Scala attributes
    val SerializableAttr = atPhase(currentRun.typerPhase)(definitions.SerializableAttr.tpe)
    val SerialVersionUID = attributeType("scala.SerialVersionUID")
    val CloneableAttr    = attributeType("scala.cloneable")
    val TransientAtt     = attributeType("scala.transient")
    val VolatileAttr     = attributeType("scala.volatile")
    val RemoteAttr       = attributeType("scala.remote")
    val ThrowsAttr       = attributeType("scala.throws")

    val CloneableClass   =
      if (forCLDC) null else definitions.getClass("java.lang.Cloneable")
    val RemoteInterface  =
      if (forCLDC) null else definitions.getClass("java.rmi.Remote")
    val RemoteException  =
      if (forCLDC) null else definitions.getClass("java.rmi.RemoteException").tpe

    var clasz: IClass = _
    var method: IMethod = _
    var code: Code = _
    var jclass: JClass = _
    var jmethod: JMethod = _
    var jcode: JExtendedCode = _

    var innerClasses: Set[Symbol] = ListSet.empty // referenced inner classes

    val fjbgContext =
      if (settings.target.value == "jvm-1.5") new FJBGContext(49, 0)
      else new FJBGContext()

    val emitSource = settings.debuginfo.level >= 1
    val emitLines  = settings.debuginfo.level >= 2
    val emitVars   = settings.debuginfo.level >= 3

    /**
     * @param jclass ...
     * @param sym    ...
     */
    def emitClass(jclass: JClass, sym: Symbol): Unit = {
      def addScalaAttr(sym: Symbol): Unit = currentRun.symData.get(sym) match {
        case Some(pickle) =>
          val scalaAttr = fjbgContext.JOtherAttribute(jclass,
                                                  jclass,
                                                  nme.ScalaSignatureATTR.toString(),
                                                  pickle.bytes,
                                                  pickle.writeIndex)
          pickledBytes = pickledBytes + pickle.writeIndex
          jclass.addAttribute(scalaAttr)
          currentRun.symData -= sym
          currentRun.symData -= sym.linkedSym
          //System.out.println("Generated ScalaSig Attr for " + sym)//debug
        case _ =>
          log("Could not find pickle information for " + sym)
      }
      if (!(jclass.getName().endsWith("$") && sym.isModuleClass))
        addScalaAttr(if (isTopLevelModule(sym)) sym.sourceModule else sym);
      addInnerClasses

      val outfile = getFile(jclass, ".class")
      jclass.writeTo(outfile)
      val file = scala.tools.nsc.io.AbstractFile.getFile(outfile)
      informProgress("wrote " + outfile)
    }

    var serialVUID: Option[Long] = None
    var remoteClass: Boolean = false

    def genClass(c: IClass): Unit = {
      if (settings.debug.value)
        log("Generating class " + c.symbol +
            " flags: " + Flags.flagsToString(c.symbol.flags))
      clasz = c
      innerClasses = ListSet.empty

      var parents = c.symbol.info.parents
      var ifaces  = JClass.NO_INTERFACES
      val name    = javaName(c.symbol)
      serialVUID  = None
      remoteClass = false

      if (parents.isEmpty)
        parents = definitions.ObjectClass.tpe :: parents;

      if (!forCLDC)
        for (val attr <- c.symbol.attributes) attr match {
          case AnnotationInfo(SerializableAttr, _, _) =>
            parents = parents ::: List(definitions.SerializableClass.tpe)
          case AnnotationInfo(CloneableAttr, _, _)  =>
            parents = parents ::: List(CloneableClass.tpe)
          case AnnotationInfo(SerialVersionUID, value :: _, _) =>
            serialVUID = Some(value.longValue)
          case AnnotationInfo(RemoteAttr, _, _) =>
            parents = parents ::: List(RemoteInterface.tpe)
            remoteClass = true
          case _ => ()
        }

      parents = parents.removeDuplicates

      if (parents.length > 1) {
        ifaces = new Array[String](parents.length - 1)
        parents.drop(1).map((s) => javaName(s.symbol)).copyToArray(ifaces, 0)
        ()
      }

      jclass = fjbgContext.JClass(javaFlags(c.symbol),
                                  name,
                                  javaName(parents(0).symbol),
                                  ifaces,
                                  c.cunit.source.toString())

      if (isStaticModule(c.symbol) || serialVUID != None) {
        if (isStaticModule(c.symbol))
            addModuleInstanceField;
        addStaticInit(jclass)

        if (isTopLevelModule(c.symbol)) {
          if (c.symbol.linkedClassOfModule == NoSymbol)
            dumpMirrorClass;
          else if (c.symbol.linkedClassOfModule != NoSymbol &&
              !currentRun.compiles(c.symbol.linkedClassOfModule)) {
            log("Dumping mirror class for " + c.symbol + " even though " +
                "linked class exists, but is not compiled in this run")
            dumpMirrorClass
          } else
            log("No mirror class for module with linked class: " +
                c.symbol.fullNameString)
        }
      }

      clasz.fields foreach genField
      clasz.methods foreach genMethod

      addAnnotations(jclass, c.symbol.attributes)

      emitClass(jclass, c.symbol)
    }

    def addExceptionsAttribute(sym: Symbol): Unit = {
      val (excs, others) = sym.attributes.partition((a => a match {
        case AnnotationInfo(ThrowsAttr, _, _) => true
        case _ => false
      }))
      if (excs isEmpty) return;
      sym.attributes = others

      val cpool = jmethod.getConstantPool()
      val buf: ByteBuffer = ByteBuffer.allocate(512)
      var nattr = 0

      // put some radom value; the actual number is determined at the end
      buf.putShort(0xbaba.toShort)

      for (val AnnotationInfo(ThrowsAttr, List(exc), _) <- excs.removeDuplicates) {
        buf.putShort(cpool.addClass(javaName(exc.typeValue.symbol)).shortValue)
        nattr = nattr + 1
      }

      assert(nattr > 0)
      buf.putShort(0, nattr.toShort)
      addAttribute(jmethod, nme.ExceptionsATTR, buf)
    }

    private def emitAttributes(buf: ByteBuffer, attributes: List[AnnotationInfo[Constant]]): Int = {
      val cpool = jclass.getConstantPool()

      def emitElement(const: Constant): Unit = const.tag match {
        case BooleanTag =>
          buf.put('Z'.toByte)
          buf.putShort(cpool.addInteger(if(const.booleanValue) 1 else 0).toShort)
        case ByteTag    =>
          buf.put('B'.toByte)
          buf.putShort(cpool.addInteger(const.byteValue).toShort)
        case ShortTag   =>
          buf.put('S'.toByte)
          buf.putShort(cpool.addInteger(const.shortValue).toShort)
        case CharTag    =>
          buf.put('C'.toByte)
          buf.putShort(cpool.addInteger(const.charValue).toShort)
        case IntTag     =>
          buf.put('I'.toByte)
          buf.putShort(cpool.addInteger(const.intValue).toShort)
        case LongTag    =>
          buf.put('J'.toByte)
          buf.putShort(cpool.addLong(const.longValue).toShort)
        case FloatTag   =>
          buf.put('F'.toByte)
          buf.putShort(cpool.addFloat(const.floatValue).toShort)
        case DoubleTag  =>
          buf.put('D'.toByte)
          buf.putShort(cpool.addDouble(const.doubleValue).toShort)
        case StringTag  =>
          buf.put('s'.toByte)
          buf.putShort(cpool.addUtf8(const.stringValue).toShort)
        case ClassTag   =>
          buf.put('c'.toByte)
          buf.putShort(cpool.addUtf8(javaType(const.typeValue).getSignature()).toShort)
        case EnumTag =>
          buf.put('e'.toByte)
          buf.putShort(cpool.addUtf8(javaType(const.tpe).getSignature()).toShort)
          buf.putShort(cpool.addUtf8(const.symbolValue.name.toString()).toShort)
        case ArrayTag =>
          buf.put('['.toByte)
          val arr = const.arrayValue
          buf.putShort(arr.length.toShort)
          for (val elem <- arr) emitElement(elem)
      }

      var nattr = 0
      val pos = buf.position()

      // put some radom value; the actual number of annotations is determined at the end
      buf.putShort(0xbaba.toShort)

      for (val AnnotationInfo(typ, consts, nvPairs) <- attributes;
           typ.symbol isNonBottomSubClass definitions.ClassfileAnnotationClass) {
        nattr = nattr + 1
        val jtype = javaType(typ)
        buf.putShort(cpool.addUtf8(jtype.getSignature()).toShort)
        assert(consts.length <= 1, consts.toString())
        buf.putShort((consts.length + nvPairs.length).toShort)
        if (!consts.isEmpty) {
          buf.putShort(cpool.addUtf8("value").toShort)
          emitElement(consts.head)
        }
        for (val (name, value) <- nvPairs) {
          buf.putShort(cpool.addUtf8(name.toString()).toShort)
          emitElement(value)
        }
      }

      // save the number of annotations
      buf.putShort(pos, nattr.toShort)
      nattr
    }

    def addAnnotations(jmember: JMember, attributes: List[AnnotationInfo[Constant]]): Unit = {
      if (attributes.isEmpty) return

      val buf: ByteBuffer = ByteBuffer.allocate(2048)

      emitAttributes(buf, attributes)

      addAttribute(jmember, nme.RuntimeAnnotationATTR, buf)
    }

    def addParamAnnotations(pattrss: List[List[AnnotationInfo[Constant]]]): Unit = {
      val attributes = for (val attrs <- pattrss) yield
        for (val attr @ AnnotationInfo(tpe, _, _) <- attrs;
             tpe.symbol isNonBottomSubClass definitions.ClassfileAnnotationClass) yield attr;
      if (attributes.forall(.isEmpty)) return;

      val buf: ByteBuffer = ByteBuffer.allocate(2048)

      // number of parameters
      buf.put(attributes.length.toByte)
      for (val attrs <- attributes)
        emitAttributes(buf, attrs)

      addAttribute(jmethod, nme.RuntimeParamAnnotationATTR, buf)
    }

    def addAttribute(jmember: JMember, name: Name, buf: ByteBuffer): Unit = {
      if (buf.position() <= 2)
        return

      val length = buf.position();
      val arr = buf.array().subArray(0, length);

      val attr = jmember.getContext().JOtherAttribute(jmember.getJClass(),
                                                      jmember,
                                                      name.toString(),
                                                      arr,
                                                      length)
      jmember.addAttribute(attr)
    }

    def addInnerClasses: Unit = {
      // add inner classes which might not have been referenced yet
      atPhase(currentRun.erasurePhase) {
        for (val sym <- clasz.symbol.info.decls.elements; sym.isClass)
          innerClasses = innerClasses + sym;
      }

      if (!innerClasses.isEmpty) {
        val innerClassesAttr = jclass.getInnerClasses();
        for (val innerSym <- innerClasses)
          innerClassesAttr.addEntry(javaName(innerSym),
              javaName(innerSym.rawowner),
              innerSym.rawname.toString(),
              javaFlags(innerSym));
      }
    }

    def isTopLevelModule(sym: Symbol): Boolean =
      atPhase (currentRun.refchecksPhase) {
        sym.isModuleClass && !sym.isImplClass && !sym.isNestedClass
      }

    def isStaticModule(sym: Symbol): Boolean = {
      sym.isModuleClass && !sym.isImplClass && !sym.hasFlag(Flags.LIFTED)
    }

    def genField(f: IField): Unit  = {
      if (settings.debug.value)
        log("Adding field: " + f.symbol.fullNameString);
      var attributes = 0

      f.symbol.attributes foreach { a => a match {
        case AnnotationInfo(TransientAtt, _, _) =>
          attributes = attributes | JAccessFlags.ACC_TRANSIENT
        case AnnotationInfo(VolatileAttr, _, _) =>
          attributes = attributes | JAccessFlags.ACC_VOLATILE
        case _ => ();
      }}
      val jfield =
        jclass.addNewField(javaFlags(f.symbol) | attributes,
                           javaName(f.symbol),
                           javaType(f.symbol.tpe));

      addAnnotations(jfield, f.symbol.attributes)
    }

    def genMethod(m: IMethod): Unit = {
      if (settings.debug.value)
        log("Generating method " + m.symbol +
            " flags: " + Flags.flagsToString(m.symbol.flags) +
            " owner: " + m.symbol.owner);
      method = m
      endPC.clear
      computeLocalVarsIndex(m)

      var resTpe = javaType(m.symbol.tpe.resultType)
      if (m.symbol.isClassConstructor)
        resTpe = JType.VOID;

      var flags = javaFlags(m.symbol)
      if (jclass.isInterface())
        flags = flags | JAccessFlags.ACC_ABSTRACT;

      jmethod = jclass.addNewMethod(flags,
                                    javaName(m.symbol),
                                    resTpe,
                                    javaTypes(m.params map (.kind)),
                                    javaNames(m.params map (.sym)));

      if (m.symbol.hasFlag(Flags.BRIDGE))
        jmethod.addAttribute(fjbgContext.JOtherAttribute(jclass, jmethod, "Bridge",
                                                         new Array[Byte](0)))
      if ((remoteClass ||
          (m.symbol.attributes contains AnnotationInfo(RemoteAttr, Nil, Nil))) &&
          jmethod.isPublic() && !forCLDC)
        {
          m.symbol.attributes =
            AnnotationInfo(ThrowsAttr, List(Constant(RemoteException)), List()) :: m.symbol.attributes;
        }

      if (!jmethod.isAbstract()) {
        jcode = jmethod.getCode().asInstanceOf[JExtendedCode]

        // add a fake local for debugging purpuses
        if (emitVars && isClosureApply(method.symbol)) {
          val outerField = clasz.symbol.info.decl(nme.getterToLocal(nme.OUTER))
          if (outerField != NoSymbol) {
            log("Adding fake local to represent outer 'this' for closure " + clasz)
            val _this = new Local(
              method.symbol.newVariable(NoPos, "this$"), toTypeKind(outerField.tpe), false)
            m.locals = m.locals ::: List(_this)
            computeLocalVarsIndex(m) // since we added a new local, we need to recompute indexes

            jcode.emitALOAD_0
            jcode.emitGETFIELD(javaName(clasz.symbol),
                               javaName(outerField),
                               javaType(outerField))
            jcode.emitSTORE(indexOf(_this), javaType(_this.kind))
          }
        }

        for (val local <- m.locals; (! m.params.contains(local))) {
          if (settings.debug.value)
            log("add local var: " + local);
          jmethod.addNewLocalVariable(javaType(local.kind), javaName(local.sym))
        }

        genCode(m)
        if (emitVars)
          genLocalVariableTable(m);
      }

      addExceptionsAttribute(m.symbol)
      addAnnotations(jmethod, m.symbol.attributes)
      addParamAnnotations(m.params.map(.sym.attributes))
    }

    def isClosureApply(sym: Symbol): Boolean = {
      (sym.name == nme.apply) &&
      sym.owner.hasFlag(Flags.SYNTHETIC) &&
      sym.owner.tpe.parents.exists { t =>
        val TypeRef(_, sym, _) = t;
        definitions.FunctionClass exists sym.==
      }
    }

    def addModuleInstanceField: Unit = {
      import JAccessFlags._
      jclass.addNewField(ACC_PUBLIC | ACC_FINAL | ACC_STATIC,
                        nme.MODULE_INSTANCE_FIELD.toString,
                        jclass.getType())
    }

    def addStaticInit(cls: JClass): Unit = {
      import JAccessFlags._
      val clinitMethod = cls.addNewMethod(ACC_PUBLIC | ACC_STATIC,
                                          "<clinit>",
                                          JType.VOID,
                                          JType.EMPTY_ARRAY,
                                          new Array[String](0))
      val clinit = clinitMethod.getCode().asInstanceOf[JExtendedCode]
      if (isStaticModule(clasz.symbol)) {
        clinit.emitNEW(cls.getName())
        clinit.emitDUP()
        clinit.emitINVOKESPECIAL(cls.getName(),
                                 JMethod.INSTANCE_CONSTRUCTOR_NAME,
                                 JMethodType.ARGLESS_VOID_FUNCTION)
      }

      serialVUID match {
        case Some(value) =>
          val fieldName = "serialVersionUID"
          jclass.addNewField(JAccessFlags.ACC_STATIC | JAccessFlags.ACC_PUBLIC,
                             fieldName,
                             JType.LONG)
          clinit.emitPUSH(value)
          clinit.emitPUTSTATIC(jclass.getName(), fieldName, JType.LONG)
        case None => ()
      }

      clinit.emitRETURN()
    }

    def dumpMirrorClass: Unit = {
      import JAccessFlags._
      assert(clasz.symbol.isModuleClass)
      if (settings.debug.value)
        log("Dumping mirror class for object: " + clasz);
      val moduleName = javaName(clasz.symbol) // + "$"
      val mirrorName = moduleName.substring(0, moduleName.length() - 1)
      val mirrorClass = fjbgContext.JClass(ACC_SUPER | ACC_PUBLIC | ACC_FINAL,
                                           mirrorName,
                                           "java.lang.Object",
                                           JClass.NO_INTERFACES,
                                           clasz.cunit.source.toString())
      for (val m <- clasz.symbol.tpe.nonPrivateMembers;
           m.owner != definitions.ObjectClass && !m.hasFlag(Flags.PROTECTED) &&
           m.isMethod && !m.hasFlag(Flags.CASE) && !m.isConstructor && !isStaticSymbol(m) )
      {
        val paramJavaTypes = m.tpe.paramTypes map (t => toTypeKind(t));
        val paramNames: Array[String] = new Array[String](paramJavaTypes.length);
        for (val i <- Iterator.range(0, paramJavaTypes.length))
          paramNames(i) = "x_" + i
        val mirrorMethod = mirrorClass
        .addNewMethod(ACC_PUBLIC | ACC_FINAL | ACC_STATIC,
                      javaName(m),
                      javaType(m.tpe.resultType),
                      javaTypes(paramJavaTypes),
                      paramNames);
        val mirrorCode = mirrorMethod.getCode().asInstanceOf[JExtendedCode];
        mirrorCode.emitGETSTATIC(moduleName,
                                 nme.MODULE_INSTANCE_FIELD.toString,
                                 new JObjectType(moduleName));
        var i = 0
        var index = 0
        var argTypes = mirrorMethod.getArgumentTypes()
        while (i < argTypes.length) {
          mirrorCode.emitLOAD(index, argTypes(i))
          index = index + argTypes(i).getSize()
          i = i + 1
        }

        mirrorCode.emitINVOKEVIRTUAL(moduleName, mirrorMethod.getName(), mirrorMethod.getType().asInstanceOf[JMethodType])
        mirrorCode.emitRETURN(mirrorMethod.getReturnType())
      }
      emitClass(mirrorClass, clasz.symbol)
    }

    var linearization: List[BasicBlock] = Nil

    var isModuleInitialized = false

    /**
     *  @param m ...
     */
    def genCode(m: IMethod): Unit = {
      labels.clear
      isModuleInitialized = false

      code = m.code
      linearization = linearizer.linearize(m)
      makeLabels(linearization)
      genBlocks(linearization)

      if (this.method.exh != Nil)
        genExceptionHandlers;
    }

    var nextBlock: BasicBlock = _

    def genBlocks(l: List[BasicBlock]): Unit = l match {
      case Nil => ()
      case x :: Nil => nextBlock = null; genBlock(x)
      case x :: y :: ys => nextBlock = y; genBlock(x); genBlocks(y :: ys)
    }


    /** Generate exception handlers for the current method. */
    def genExceptionHandlers: Unit = {

      /** Return a list of pairs of intervals where the handler is active.
       *  The intervals in the list have to be inclusive in the beginning and
       *  exclusive in the end: [start, end).
       */
      def ranges(e: ExceptionHandler): List[(Int, Int)] = {
        var covered = e.covered
        var ranges: List[(Int, Int)] = Nil
        var start = -1
        var end = -1

        linearization foreach ((b) => {
          if (! (covered contains b) ) {
            if (start >= 0) { // we're inside a handler range
              end = labels(b).getAnchor()
              ranges = (start, end) :: ranges
              start = -1
            }
          } else {
            if (start >= 0) { // we're inside a handler range
              end = endPC(b)
            } else {
              start = labels(b).getAnchor()
              end   = endPC(b)
            }
            covered = covered remove b.==
          }
        });

        /* Add the last interval. Note that since the intervals are
         * open-ended to the right, we have to give a number past the actual
         * code!
         */
        if (start >= 0) {
          ranges = (start, jcode.getPC()) :: ranges;
        }

        if (covered != Nil)
          if (settings.debug.value)
            log("Some covered blocks were not found in method: " + method +
                " covered: " + covered + " not in " + linearization);
        ranges
      }

      this.method.exh foreach ((e) => {
        ranges(e).sort({ (p1, p2) => p1._1 < p2._1 })
        .foreach ((p) => {
          if (settings.debug.value)
            log("Adding exception handler " + e + "at block: " + e.startBlock + " for " + method +
                " from: " + p._1 + " to: " + p._2 + " catching: " + e.cls);
          jcode.addExceptionHandler(p._1, p._2,
                                    labels(e.startBlock).getAnchor(),
                                    if (e.cls == NoSymbol)
                                      null
                                    else javaName(e.cls))
        })
      });
    }

    /** local variables whose scope appears in this block. */
    var varsInBlock: collection.mutable.Set[Local] = new HashSet

    def genBlock(b: BasicBlock): Unit = {
      labels(b).anchorToNext()

      if (settings.debug.value)
        log("Generating code for block: " + b + " at pc: " + labels(b).getAnchor());
      var lastMappedPC = 0
      var lastLineNr = 0
      var crtPC = 0
      varsInBlock.clear

      b traverse ( instr => {
        class CompilationError(msg: String) extends Error {
          override def toString(): String = {
            msg +
            "\nCurrent method: " + method +
            "\nCurrent block: " + b +
            "\nCurrent instruction: " + instr +
            "\n---------------------" +
            method.dump
          }
        }
        def assert(cond: Boolean, msg: String) = if (!cond) throw new CompilationError(msg);

        instr match {
          case THIS(clasz) =>
            jcode.emitALOAD_0()

          case CONSTANT(const) =>
            const.tag match {
              case UnitTag    => ();
              case BooleanTag => jcode.emitPUSH(const.booleanValue)
              case ByteTag    => jcode.emitPUSH(const.byteValue)
              case ShortTag   => jcode.emitPUSH(const.shortValue)
              case CharTag    => jcode.emitPUSH(const.charValue)
              case IntTag     => jcode.emitPUSH(const.intValue)
              case LongTag    => jcode.emitPUSH(const.longValue)
              case FloatTag   => jcode.emitPUSH(const.floatValue)
              case DoubleTag  => jcode.emitPUSH(const.doubleValue)
              case StringTag  => jcode.emitPUSH(const.stringValue)
              case NullTag    => jcode.emitACONST_NULL()
              case ClassTag   =>
                val kind = toTypeKind(const.typeValue);
                if (kind.isValueType)
                  jcode.emitPUSH(classLiteral(kind));
                else
                  jcode.emitPUSH(javaType(kind).asInstanceOf[JReferenceType]);
              case EnumTag   =>
                val sym = const.symbolValue
                jcode.emitGETSTATIC(javaName(sym.owner),
                                    javaName(sym),
                                    javaType(const.tpe))
              case _          => abort("Unknown constant value: " + const);
            }

          case LOAD_ARRAY_ITEM(kind) =>
            jcode.emitALOAD(javaType(kind))

          case LOAD_LOCAL(local) =>
            jcode.emitLOAD(indexOf(local), javaType(local.kind))

          case LOAD_FIELD(field, isStatic) =>
            var owner = javaName(field.owner);
//            if (field.owner.hasFlag(Flags.MODULE)) owner = owner + "$";
            if (settings.debug.value)
              log("LOAD_FIELD with owner: " + owner +
                  " flags: " + Flags.flagsToString(field.owner.flags))
            if (isStatic)
              jcode.emitGETSTATIC(owner,
                                  javaName(field),
                                  javaType(field))
            else
              jcode.emitGETFIELD(owner,
                                  javaName(field),
                                  javaType(field))

          case LOAD_MODULE(module) =>
            assert(module.isModule || module.isModuleClass, "Expected module: " + module)
            if (settings.debug.value)
              log("genearting LOAD_MODULE for: " + module + " flags: " +
                  Flags.flagsToString(module.flags));
            jcode.emitGETSTATIC(javaName(module) /* + "$" */ ,
                                nme.MODULE_INSTANCE_FIELD.toString,
                                javaType(module));

          case STORE_ARRAY_ITEM(kind) =>
            jcode.emitASTORE(javaType(kind))

          case STORE_LOCAL(local) =>
            jcode.emitSTORE(indexOf(local), javaType(local.kind))

          case STORE_FIELD(field, isStatic) =>
            val owner = javaName(field.owner) // + (if (field.owner.hasFlag(Flags.MODULE)) "$" else "");
            if (isStatic)
              jcode.emitPUTSTATIC(owner,
                                  javaName(field),
                                  javaType(field))
            else
              jcode.emitPUTFIELD(owner,
                                  javaName(field),
                                  javaType(field))

          case CALL_PRIMITIVE(primitive) =>
            genPrimitive(primitive, instr.pos)

          case call @ CALL_METHOD(method, style) =>
            val owner: String = javaName(method.owner);
            //reference the type of the receiver instead of the method owner (if not an interface!)
            val dynamicOwner =
              if (needsInterfaceCall(call.hostClass)) owner
              else javaName(call.hostClass)

            style match {
              case Dynamic =>
                if (needsInterfaceCall(method.owner))
                  jcode.emitINVOKEINTERFACE(owner,
                                            javaName(method),
                                            javaType(method).asInstanceOf[JMethodType])
                else
                  jcode.emitINVOKEVIRTUAL(dynamicOwner,
                                          javaName(method),
                                          javaType(method).asInstanceOf[JMethodType]);

              case Static(instance) =>
                if (instance) {
                  jcode.emitINVOKESPECIAL(owner,
                                          javaName(method),
                                          javaType(method).asInstanceOf[JMethodType]);
                } else
                  jcode.emitINVOKESTATIC(owner,
                                          javaName(method),
                                          javaType(method).asInstanceOf[JMethodType]);

              case SuperCall(_) =>
                  jcode.emitINVOKESPECIAL(owner,
                                          javaName(method),
                                          javaType(method).asInstanceOf[JMethodType]);
                  // we initialize the MODULE$ field immediately after the super ctor
                  if (isStaticModule(clasz.symbol) && !isModuleInitialized &&
                      jmethod.getName() == JMethod.INSTANCE_CONSTRUCTOR_NAME &&
                      javaName(method) == JMethod.INSTANCE_CONSTRUCTOR_NAME) {
                        isModuleInitialized = true;
                        jcode.emitALOAD_0();
                        jcode.emitPUTSTATIC(jclass.getName(),
                                            nme.MODULE_INSTANCE_FIELD.toString,
                                            jclass.getType());
                      }


            }

          case BOX(kind) =>
            val boxedClass = definitions.boxedClass(kind.toType.symbol)
            val mtype = new JMethodType(javaType(boxedClass), Array(javaType(kind)))
            jcode.emitINVOKESTATIC(javaName(boxedClass), "box", mtype)

          case UNBOX(BOOL) /* if (boxType.symbol == definitions.BooleanClass) */ =>
            // if null emit false
            val nonNull = jcode.newLabel()
            jcode.emitDUP()
            jcode.emitIFNONNULL(nonNull)
            jcode.emitPOP()
            jcode.emitPUSH(false)
            val lexit = jcode.newLabel()
            jcode.emitGOTO(lexit)
            nonNull.anchorToNext()
            // else unbox the reference at the top of the stack
            val boxedBoolean = javaName(definitions.boxedClass(definitions.BooleanClass))
            jcode.emitCHECKCAST(new JObjectType(boxedBoolean))
            jcode.emitGETFIELD(boxedBoolean, "value", JType.BOOLEAN)
            lexit.anchorToNext()

          case UNBOX(boxKind) =>
            // if null emit a zero of the appropriate kind
            val nonNull = jcode.newLabel()
            jcode.emitDUP()
            jcode.emitIFNONNULL(nonNull)
            jcode.emitPOP()
            (boxKind: @unchecked) match {
              case BYTE            => jcode.emitPUSH(0: Byte)
              case SHORT           => jcode.emitPUSH(0: Short)
              case CHAR            => jcode.emitPUSH(0: Char)
              case INT             => jcode.emitPUSH(0: Int)
              case LONG            => jcode.emitPUSH(0: Long)
              case FLOAT           => jcode.emitPUSH(0.0f)
              case DOUBLE          => jcode.emitPUSH(0.0d)
            }
            val lexit = jcode.newLabel()
            jcode.emitGOTO(lexit)
            nonNull.anchorToNext()
            // else unbox the reference at the top of the stack
            jcode.emitCHECKCAST(new JObjectType(BOXED_NUMBER))
            val clazzName = boxKind.toType.symbol.name.toString()
            val unboxMethod = clazzName.toLowerCase() + "Value"
            val mtype = new JMethodType(javaType(boxKind), new Array[JType](0))
            jcode.emitINVOKEVIRTUAL(BOXED_NUMBER, unboxMethod, mtype)
            lexit.anchorToNext()

          case NEW(REFERENCE(cls)) =>
            val className = javaName(cls)
            jcode.emitNEW(className)

          case CREATE_ARRAY(elem) => elem match {
            case REFERENCE(_) | ARRAY(_) =>
              jcode.emitANEWARRAY(javaType(elem).asInstanceOf[JReferenceType])
            case _ =>
              jcode.emitNEWARRAY(javaType(elem))
          }

          case IS_INSTANCE(tpe) =>
            tpe match {
              case REFERENCE(cls) => jcode.emitINSTANCEOF(new JObjectType(javaName(cls)))
              case ARRAY(elem)    => jcode.emitINSTANCEOF(new JArrayType(javaType(elem)))
              case _ => abort("Unknown reference type in IS_INSTANCE: " + tpe)
            }

          case CHECK_CAST(tpe) =>
            tpe match {
              case REFERENCE(cls) => jcode.emitCHECKCAST(new JObjectType(javaName(cls)))
              case ARRAY(elem)    => jcode.emitCHECKCAST(new JArrayType(javaType(elem)))
              case _ => abort("Unknown reference type in IS_INSTANCE: " + tpe)
            }

          case SWITCH(tags, branches) =>
            val tagArray = new Array[Array[Int]](tags.length)
            var caze = tags
            var i = 0

            while (i < tagArray.length) {
              tagArray(i) = new Array[Int](caze.head.length)
              caze.head.copyToArray(tagArray(i), 0)
              i = i + 1
              caze = caze.tail
            }
            val branchArray = new Array[JCode$Label](tagArray.length)
            if (settings.debug.value)
              log("Emitting SWITHCH:\ntags: " + tags + "\nbranches: " + branches);
            jcode.emitSWITCH(tagArray,
                             { (branches map labels dropRight 1).copyToArray(branchArray, 0);
                              branchArray },
                             labels(branches.last),
                             MIN_SWITCH_DENSITY);

          case JUMP(where) =>
            if (nextBlock != where)
              jcode.emitGOTO_maybe_W(labels(where), false); // default to short jumps

          case CJUMP(success, failure, cond, kind) =>
            kind match {
              case BOOL | BYTE | CHAR | SHORT | INT =>
                if (nextBlock == success) {
                  jcode.emitIF_ICMP(conds(negate(cond)), labels(failure))
                  // .. and fall through to success label
                } else {
                  jcode.emitIF_ICMP(conds(cond), labels(success))
                  if (nextBlock != failure)
                    jcode.emitGOTO_maybe_W(labels(failure), false);
                }

              case REFERENCE(_) | ARRAY(_) =>
                if (nextBlock == success) {
                  jcode.emitIF_ACMP(conds(negate(cond)), labels(failure))
                  // .. and fall through to success label
                } else {
                  jcode.emitIF_ACMP(conds(cond), labels(success))
                  if (nextBlock != failure)
                    jcode.emitGOTO_maybe_W(labels(failure), false);
                }

              case _ =>
                (kind: @unchecked) match {
                  case LONG   => jcode.emitLCMP()
                  case FLOAT  => jcode.emitFCMPG()
                  case DOUBLE => jcode.emitDCMPG()
                }
                if (nextBlock == success) {
                  jcode.emitIF(conds(negate(cond)), labels(failure));
                  // .. and fall through to success label
                } else {
                  jcode.emitIF(conds(cond), labels(success));
                  if (nextBlock != failure)
                    jcode.emitGOTO_maybe_W(labels(failure), false);
                }
            }

          case CZJUMP(success, failure, cond, kind) =>
            kind match {
              case BOOL | BYTE | CHAR | SHORT | INT =>
                if (nextBlock == success) {
                  jcode.emitIF(conds(negate(cond)), labels(failure));
                } else {
                  jcode.emitIF(conds(cond), labels(success))
                  if (nextBlock != failure)
                    jcode.emitGOTO_maybe_W(labels(failure), false);
                }

              case REFERENCE(_) | ARRAY(_) =>
                if (nextBlock == success) {
                  jcode.emitIFNONNULL(labels(failure))
                } else {
                  jcode.emitIFNULL(labels(success));
                  if (nextBlock != failure)
                    jcode.emitGOTO_maybe_W(labels(failure), false);
                }

              case _ =>
                (kind: @unchecked) match {
                  case LONG   => jcode.emitLCONST_0(); jcode.emitLCMP()
                  case FLOAT  => jcode.emitFCONST_0(); jcode.emitFCMPL()
                  case DOUBLE => jcode.emitDCONST_0(); jcode.emitDCMPL()
                }
                if (nextBlock == success) {
                  jcode.emitIF(conds(negate(cond)), labels(failure))
                } else {
                  jcode.emitIF(conds(cond), labels(success))
                  if (nextBlock != failure)
                    jcode.emitGOTO_maybe_W(labels(failure), false);
                }
            }

          case RETURN(kind) =>
            jcode.emitRETURN(javaType(kind))

          case THROW() =>
            jcode.emitATHROW()

          case DROP(kind) =>
            kind match {
              case LONG | DOUBLE => jcode.emitPOP2()
              case _ => jcode.emitPOP()
            }

          case DUP(kind) =>
            kind match {
              case LONG | DOUBLE => jcode.emitDUP2()
              case _ => jcode.emitDUP()
            }

          case MONITOR_ENTER() =>
            jcode.emitMONITORENTER()

          case MONITOR_EXIT() =>
            jcode.emitMONITOREXIT()

          case SCOPE_ENTER(lv) =>
            varsInBlock += lv
            lv.start = jcode.getPC()

          case SCOPE_EXIT(lv) =>
            if (varsInBlock contains lv) {
              lv.ranges = (lv.start, jcode.getPC()) :: lv.ranges
              varsInBlock -= lv
            } else if (b.varsInScope contains lv) {
              lv.ranges = (labels(b).getAnchor(), jcode.getPC()) :: lv.ranges
              b.varsInScope -= lv
            } else
              assert(false, "Illegal local var nesting: " + method)
        }

        crtPC = jcode.getPC()
        val crtLine = try {
          clasz.cunit.position(instr.pos).line
        } catch {
          case _: Error =>
            log("Warning: wrong position in: " + method)
            lastLineNr
        }
        if (b.lastInstruction == instr)
          endPC(b) = jcode.getPC();

	//System.err.println("CRTLINE: " + instr.pos + " " +
	//	   /* (if (instr.pos < clasz.cunit.source.content.length) clasz.cunit.source.content(instr.pos) else '*') + */ " " + crtLine);

        if (crtPC > lastMappedPC) {
          jcode.completeLineNumber(lastMappedPC, crtPC, crtLine)
          lastMappedPC = crtPC
          lastLineNr   = crtLine
        }

      }); // b.traverse

      // local vars that survived this basic block
      for (val lv <- varsInBlock) {
        lv.ranges = (lv.start, jcode.getPC()) :: lv.ranges
      }
      for (val lv <- b.varsInScope) {
        lv.ranges = (labels(b).getAnchor(), jcode.getPC()) :: lv.ranges
      }
    }

    /**
     *  @param primitive ...
     *  @param pos       ...
     */
    def genPrimitive(primitive: Primitive, pos: Int): Unit = {
      primitive match {
        case Negation(kind) =>
          kind match {
            case BOOL | BYTE | CHAR | SHORT | INT =>
              jcode.emitINEG()

            case LONG   => jcode.emitLNEG()
            case FLOAT  => jcode.emitFNEG()
            case DOUBLE => jcode.emitDNEG()
            case _ => abort("Impossible to negate a " + kind)
          }

        case Arithmetic(op, kind) =>
          op match {
            case ADD => jcode.emitADD(javaType(kind))
            case SUB =>
              (kind: @unchecked) match {
                case BOOL | BYTE | CHAR | SHORT | INT =>
                  jcode.emitISUB()
                case LONG   => jcode.emitLSUB()
                case FLOAT  => jcode.emitFSUB()
                case DOUBLE => jcode.emitDSUB()
              }

            case MUL =>
              (kind: @unchecked) match {
                case BOOL | BYTE | CHAR | SHORT | INT =>
                  jcode.emitIMUL()
                case LONG   => jcode.emitLMUL()
                case FLOAT  => jcode.emitFMUL()
                case DOUBLE => jcode.emitDMUL()
              }

            case DIV =>
              (kind: @unchecked) match {
                case BOOL | BYTE | CHAR | SHORT | INT =>
                  jcode.emitIDIV()
                case LONG   => jcode.emitLDIV()
                case FLOAT  => jcode.emitFDIV()
                case DOUBLE => jcode.emitDDIV()
              }

            case REM =>
              (kind: @unchecked) match {
                case BOOL | BYTE | CHAR | SHORT | INT =>
                  jcode.emitIREM()
                case LONG   => jcode.emitLREM()
                case FLOAT  => jcode.emitFREM()
                case DOUBLE => jcode.emitDREM()
              }

            case NOT =>
              kind match {
                case BOOL | BYTE | CHAR | SHORT | INT =>
                  jcode.emitPUSH(-1)
                  jcode.emitIXOR()
                case LONG   =>
                  jcode.emitPUSH(-1l)
                  jcode.emitLXOR()
                case _ =>
                  abort("Impossible to negate an " + kind)
              }

            case _ =>
              abort("Unknown arithmetic primitive " + primitive)
          }

        case Logical(op, kind) => (op, kind) match {
          case (AND, LONG) =>
            jcode.emitLAND()
          case (AND, INT) =>
            jcode.emitIAND()
          case (AND, _) =>
            jcode.emitIAND()
            if (kind != BOOL)
              jcode.emitT2T(javaType(INT), javaType(kind));

          case (OR, LONG) =>
            jcode.emitLOR()
          case (OR, INT) =>
            jcode.emitIOR()
          case (OR, _) =>
            jcode.emitIOR()
            if (kind != BOOL)
              jcode.emitT2T(javaType(INT), javaType(kind));

          case (XOR, LONG) =>
            jcode.emitLXOR()
          case (XOR, INT) =>
            jcode.emitIXOR()
          case (XOR, _) =>
            jcode.emitIXOR()
            if (kind != BOOL)
              jcode.emitT2T(javaType(INT), javaType(kind));
        }

        case Shift(op, kind) => (op, kind) match {
          case (LSL, LONG) =>
            jcode.emitLSHL()
          case (LSL, INT) =>
            jcode.emitISHL()
          case (LSL, _) =>
            jcode.emitISHL()
            jcode.emitT2T(javaType(INT), javaType(kind))

          case (ASR, LONG) =>
            jcode.emitLSHR()
          case (ASR, INT) =>
            jcode.emitISHR()
          case (ASR, _) =>
            jcode.emitISHR()
            jcode.emitT2T(javaType(INT), javaType(kind))

          case (LSR, LONG) =>
            jcode.emitLUSHR()
          case (LSR, INT) =>
            jcode.emitIUSHR()
          case (LSR, _) =>
            jcode.emitIUSHR()
            jcode.emitT2T(javaType(INT), javaType(kind))
        }

        case Comparison(op, kind) => ((op, kind): @unchecked) match {
          case (CMP, LONG)    => jcode.emitLCMP()
          case (CMPL, FLOAT)  => jcode.emitFCMPL()
          case (CMPG, FLOAT)  => jcode.emitFCMPG()
          case (CMPL, DOUBLE) => jcode.emitDCMPL()
          case (CMPG, DOUBLE) => jcode.emitDCMPL()
        }

        case Conversion(src, dst) =>
          if (settings.debug.value)
            log("Converting from: " + src + " to: " + dst);
          if (dst == BOOL) {
            Console.println("Illegal conversion at: " + clasz +
                            " at: " + method.sourceFile + ":" + Position.line(clasz.cunit.source, pos));
          } else
            jcode.emitT2T(javaType(src), javaType(dst));

        case ArrayLength(_) =>
          jcode.emitARRAYLENGTH()

        case StartConcat =>
          jcode.emitNEW(JAVA_LANG_STRINGBUFFER)
          jcode.emitDUP()
          jcode.emitINVOKESPECIAL(JAVA_LANG_STRINGBUFFER,
                                  JMethod.INSTANCE_CONSTRUCTOR_NAME,
                                  JMethodType.ARGLESS_VOID_FUNCTION)

        case StringConcat(el) =>
          val jtype = el match {
            case REFERENCE(_) | ARRAY(_)=> JObjectType.JAVA_LANG_OBJECT
            case _ => javaType(el)
          }
          jcode.emitINVOKEVIRTUAL(JAVA_LANG_STRINGBUFFER,
                                  "append",
                                  new JMethodType(stringBufferType,
                                  Array(jtype)))
        case EndConcat =>
          jcode.emitINVOKEVIRTUAL(JAVA_LANG_STRINGBUFFER,
                                  "toString",
                                  toStringType)

        case _ =>
          abort("Unimplemented primitive " + primitive)
      }
    }

    /** For each basic block, the first PC address following it. */
    val endPC: HashMap[BasicBlock, Int] = new HashMap()
    val labels: HashMap[BasicBlock, JCode$Label] = new HashMap()
    val conds: HashMap[TestOp, Int] = new HashMap()

    conds += EQ -> JExtendedCode.COND_EQ
    conds += NE -> JExtendedCode.COND_NE
    conds += LT -> JExtendedCode.COND_LT
    conds += GT -> JExtendedCode.COND_GT
    conds += LE -> JExtendedCode.COND_LE
    conds += GE -> JExtendedCode.COND_GE

    val negate: HashMap[TestOp, TestOp] = new HashMap()

    negate += EQ -> NE
    negate += NE -> EQ
    negate += LT -> GE
    negate += GT -> LE
    negate += LE -> GT
    negate += GE -> LT

    /** Map from type kinds to the Java reference types. It is used for
     *  loading class constants. @see Predef.classOf. */
    val classLiteral: Map[TypeKind, JObjectType] = new HashMap()

    classLiteral += UNIT   -> new JObjectType("java.lang.Void")
    classLiteral += BOOL   -> new JObjectType("java.lang.Boolean")
    classLiteral += BYTE   -> new JObjectType("java.lang.Byte")
    classLiteral += SHORT  -> new JObjectType("java.lang.Short")
    classLiteral += CHAR   -> new JObjectType("java.lang.Character")
    classLiteral += INT    -> new JObjectType("java.lang.Integer")
    classLiteral += LONG   -> new JObjectType("java.lang.Long")
    classLiteral += FLOAT  -> new JObjectType("java.lang.Float")
    classLiteral += DOUBLE -> new JObjectType("java.lang.Double")

    def makeLabels(bs: List[BasicBlock]) = {
      //labels.clear
      if (settings.debug.value)
        log("Making labels for: " + method);
      bs foreach (bb => labels += bb -> jcode.newLabel() )
    }


    ////////////////////// local vars ///////////////////////

    def sizeOf(sym: Symbol): Int = sizeOf(toTypeKind(sym.tpe))


    def sizeOf(k: TypeKind): Int = k match {
      case DOUBLE | LONG => 2
      case _ => 1
    }

    def indexOf(m: IMethod, sym: Symbol): Int = {
      val Some(local) = m.lookupLocal(sym)
      assert(local.index >= 0,
             "Invalid index for: " + local + "{" + local.hashCode + "}")
      local.index
    }

    def indexOf(local: Local): Int = {
      assert(local.index >= 0,
             "Invalid index for: " + local + "{" + local.hashCode + "}")
      local.index
    }

    /**
     * Compute the indexes of each local variable of the given
     * method.
     */
    def computeLocalVarsIndex(m: IMethod): Unit = {
      var idx = 1
      if (isStaticSymbol(m.symbol))
        idx = 0;

      for (val l <- m.locals) {
        if (settings.debug.value)
          log("Index value for " + l + "{" + l.hashCode + "}: " + idx)
        l.index = idx
        idx = idx + sizeOf(l.kind)
      }
    }

    ////////////////////// Utilities ////////////////////////

    /**
     * <p>
     *   Return the a name of this symbol that can be used on the Java
     *   platform. It removes spaces from names.
     * </p>
     * <p>
     *   Special handling: scala.Nothing and <code>scala.Null</code> are
     *   <em>erased</em> to <code>scala.runtime.Nothing$</code> and
     *   </code>scala.runtime.Null$</code>. This is needed because they are
     *   not real classes, and they mean 'abrupt termination upon evaluation
     *   of that expression' or <code>null</code> respectively. This handling is
     *   done already in <a href="../icode/GenIcode.html" target="contentFrame">
     *   <code>GenICode</code></a>, but here we need to remove references
     *   from method signatures to these types, because such classes can
     *   not exist in the classpath: the type checker will be very confused.
     * </p>
     */
    def javaName(sym: Symbol): String = {
      val suffix = if (sym.hasFlag(Flags.MODULE) && !sym.isMethod &&
                        !sym.isImplClass &&
                        !sym.hasFlag(Flags.JAVA)) "$" else "";

      if (sym == definitions.AllClass)
        return "scala.runtime.Nothing$"
      else if (sym == definitions.AllRefClass)
        return "scala.runtime.Null$"

      if (sym.isClass && !sym.rawowner.isPackageClass)
        innerClasses = innerClasses + sym;

      (if (sym.isClass || (sym.isModule && !sym.isMethod))
        sym.fullNameString('/')
      else
        sym.simpleName.toString().trim()) + suffix
    }

    def javaNames(syms: List[Symbol]): Array[String] = {
      val res = new Array[String](syms.length)
      var i = 0
      syms foreach (s => { res(i) = javaName(s); i = i + 1; })
      res
    }

    /**
     * Return the Java modifiers for the given symbol.
     * Java modifiers for classes:
     *  - public, abstract, final, strictfp (not used)
     * for interfaces:
     *  - the same as for classes, without 'final'
     * for fields:
     *  - public, private (*)
     *  - static, final
     * for methods:
     *  - the same as for fields, plus:
     *  - abstract, synchronized (not used), strictfp (not used), native (not used)
     *
     *  (*) protected cannot be used, since inner classes 'see' protected members,
     *      and they would fail verification after lifted.
     */
    def javaFlags(sym: Symbol): Int = {
      import JAccessFlags._

      var jf: Int = 0
      val f = sym.flags
      jf = jf | (if (sym hasFlag Flags.SYNTHETIC) ACC_SYNTHETIC else 0)
/*      jf = jf | (if (sym hasFlag Flags.PRIVATE) ACC_PRIVATE else
                  if (sym hasFlag Flags.PROTECTED) ACC_PROTECTED else ACC_PUBLIC)
*/
      jf = jf | (if (sym hasFlag Flags.PRIVATE) ACC_PRIVATE else  ACC_PUBLIC)
      jf = jf | (if ((sym hasFlag Flags.ABSTRACT) ||
                     (sym hasFlag Flags.DEFERRED)) ACC_ABSTRACT else 0)
      jf = jf | (if (sym hasFlag Flags.INTERFACE) ACC_INTERFACE else 0)
      jf = jf | (if ((sym hasFlag Flags.FINAL) && !sym.enclClass.hasFlag(Flags.INTERFACE)) ACC_FINAL else 0)
      jf = jf | (if (isStaticSymbol(sym)) ACC_STATIC else 0)
      jf = jf | (if (sym hasFlag Flags.SYNTHETIC) ACC_SYNTHETIC else 0)
      jf
    }

    def isStaticSymbol(s: Symbol): Boolean =
      s.hasFlag(Flags.STATIC) || s.hasFlag(Flags.STATICMEMBER) || s.owner.isImplClass;

    /** Calls to methods in 'sym' need invokeinterface? */
    def needsInterfaceCall(sym: Symbol): Boolean =
      sym.hasFlag(Flags.INTERFACE) ||
      (sym.hasFlag(Flags.JAVA) &&
       sym.isNonBottomSubClass(definitions.ClassfileAnnotationClass))


    def javaType(t: TypeKind): JType = (t: @unchecked) match {
      case UNIT            => JType.VOID
      case BOOL            => JType.BOOLEAN
      case BYTE            => JType.BYTE
      case SHORT           => JType.SHORT
      case CHAR            => JType.CHAR
      case INT             => JType.INT
      case LONG            => JType.LONG
      case FLOAT           => JType.FLOAT
      case DOUBLE          => JType.DOUBLE
      case REFERENCE(cls)  => new JObjectType(javaName(cls))
      case ARRAY(elem)     => new JArrayType(javaType(elem))
    }

    def javaType(t: Type): JType = javaType(toTypeKind(t))

    def javaType(s: Symbol): JType =
      if (s.isMethod)
        new JMethodType(
          if (s.isClassConstructor) JType.VOID else javaType(s.tpe.resultType),
          s.tpe.paramTypes.map(javaType).toArray)
      else
        javaType(s.tpe)

    def javaTypes(ts: List[TypeKind]): Array[JType] = {
      val res = new Array[JType](ts.length)
      var i = 0
      ts foreach ( t => { res(i) = javaType(t); i = i + 1; } );
      res
    }

    def getFile(cls: JClass, suffix: String): String = {
      val path = cls.getName().replace('.', File.separatorChar)
      settings.outdir.value + File.separatorChar + path + suffix
    }

    /** Emit a Local variable table for debugging purposes.
     *  Synthetic locals are skipped. All variables are method-scoped.
     */
    private def genLocalVariableTable(m: IMethod): Unit = {
        var vars = m.locals.filter(l => !l.sym.hasFlag(Flags.SYNTHETIC))

        if (vars.length == 0) return

        val pool = jclass.getConstantPool()
        val pc = jcode.getPC()
        var anonCounter = 0
        var entries = 0
        vars.foreach { lv => lv.ranges = mergeEntries(lv.ranges.reverse); entries = entries + lv.ranges.length }
        if (!jmethod.isStatic()) entries = entries + 1

        val lvTab = ByteBuffer.allocate(2 + 10 * entries)
        def emitEntry(name: String, signature: String, idx: Short, start: Short, end: Short): Unit = {
          lvTab.putShort(start)
          lvTab.putShort(end)
          lvTab.putShort(pool.addUtf8(name).asInstanceOf[Short])
          lvTab.putShort(pool.addUtf8(signature).asInstanceOf[Short])
          lvTab.putShort(idx)
        }

        lvTab.putShort(entries.asInstanceOf[Short])

        if (!jmethod.isStatic()) {
          emitEntry("this", jclass.getType().getSignature(), 0, 0.asInstanceOf[Short], pc.asInstanceOf[Short])
        }

        for (val lv <- vars) {
            val name = if (javaName(lv.sym) eq null) {
              anonCounter = anonCounter + 1
              "<anon" + anonCounter + ">"
            } else javaName(lv.sym)

            val index = indexOf(lv).asInstanceOf[Short]
            val tpe   = javaType(lv.kind).getSignature()
            for (val (start, end) <- lv.ranges) {
              emitEntry(name, tpe, index, start.asInstanceOf[Short], (end - start).asInstanceOf[Short])
            }
        }
        val attr =
            fjbgContext.JOtherAttribute(jclass,
                                        jmethod,
                                        "LocalVariableTable",
                                        lvTab.array())
        jcode.addAttribute(attr)
    }


    /** Merge adjacent ranges. */
    private def mergeEntries(ranges: List[(Int, Int)]): List[(Int, Int)] =
      (ranges.foldLeft(Nil: List[(Int, Int)]) { (collapsed: List[(Int, Int)], p: (Int, Int)) => (collapsed, p) match {
        case (Nil, _) => List(p)
        case ((s1, e1) :: rest, (s2, e2)) if (e1 == s2) => (s1, e2) :: rest
        case _ => p :: collapsed
      }}).reverse

    def assert(cond: Boolean, msg: String) = if (!cond) {
      method.dump
      throw new Error(msg + "\nMethod: " + method)
    }

    def assert(cond: Boolean): Unit = assert(cond, "Assertion failed.")
  }
}
