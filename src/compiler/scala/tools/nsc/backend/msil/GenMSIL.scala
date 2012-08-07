/* NSC -- new scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author Nikolay Mihaylov
 */


package scala.tools.nsc
package backend.msil

import java.io.{File, IOException}
import java.nio.{ByteBuffer, ByteOrder}
import scala.collection.{ mutable, immutable }
import scala.tools.nsc.symtab._

import ch.epfl.lamp.compiler.msil.{Type => MsilType, _}
import ch.epfl.lamp.compiler.msil.emit._
import ch.epfl.lamp.compiler.msil.util.PECustomMod
import language.postfixOps

abstract class GenMSIL extends SubComponent {
  import global._
  import loaders.clrTypes
  import clrTypes.{types, constructors, methods, fields}
  import icodes._
  import icodes.opcodes._

  val x = loaders

  /** Create a new phase */
  override def newPhase(p: Phase) = new MsilPhase(p)

  val phaseName = "msil"
  /** MSIL code generation phase
   */
  class MsilPhase(prev: Phase) extends GlobalPhase(prev) {
    def name = phaseName
    override def newFlags = phaseNewFlags

    override def erasedTypes = true

    override def run() {
      if (settings.debug.value) inform("[running phase " + name + " on icode]")

      val codeGenerator = new BytecodeGenerator

      //classes is ICodes.classes, a HashMap[Symbol, IClass]
      classes.values foreach codeGenerator.findEntryPoint
      if( opt.showClass.isDefined && (codeGenerator.entryPoint == null) ) { // TODO introduce dedicated setting instead
        val entryclass = opt.showClass.get.toString
        warning("Couldn't find entry class " + entryclass)
      }

      codeGenerator.initAssembly

      val classesSorted = classes.values.toList.sortBy(c => c.symbol.id) // simplifies comparing cross-compiler vs. .exe output
      classesSorted foreach codeGenerator.createTypeBuilder
      classesSorted foreach codeGenerator.createClassMembers

      try {
        classesSorted foreach codeGenerator.genClass
      } finally {
        codeGenerator.writeAssembly
      }
    }

    override def apply(unit: CompilationUnit) {
      abort("MSIL works on icode classes, not on compilation units!")
    }
  }

  /**
   * MSIL bytecode generator.
   *
   */
  class BytecodeGenerator {

    val MODULE_INSTANCE_NAME = "MODULE$"

    import clrTypes.{VOID => MVOID, BOOLEAN => MBOOL, BYTE => MBYTE, SHORT => MSHORT,
                   CHAR => MCHAR, INT => MINT, LONG => MLONG, FLOAT => MFLOAT,
                   DOUBLE => MDOUBLE, OBJECT => MOBJECT, STRING => MSTRING,
                   STRING_ARRAY => MSTRING_ARRAY,
                   SYMTAB_CONSTR => SYMTAB_ATTRIBUTE_CONSTRUCTOR,
                   SYMTAB_DEFAULT_CONSTR => SYMTAB_ATTRIBUTE_EMPTY_CONSTRUCTOR}

    val EXCEPTION = clrTypes.getType("System.Exception")
    val MBYTE_ARRAY = clrTypes.mkArrayType(MBYTE)

    val ICLONEABLE = clrTypes.getType("System.ICloneable")
    val MEMBERWISE_CLONE = MOBJECT.GetMethod("MemberwiseClone", MsilType.EmptyTypes)

    val MMONITOR       = clrTypes.getType("System.Threading.Monitor")
    val MMONITOR_ENTER = MMONITOR.GetMethod("Enter", Array(MOBJECT))
    val MMONITOR_EXIT  = MMONITOR.GetMethod("Exit", Array(MOBJECT))

    val MSTRING_BUILDER = clrTypes.getType("System.Text.StringBuilder")
    val MSTRING_BUILDER_CONSTR = MSTRING_BUILDER.GetConstructor(MsilType.EmptyTypes)
    val MSTRING_BUILDER_TOSTRING = MSTRING_BUILDER.GetMethod("ToString",
                                                             MsilType.EmptyTypes)

    val TYPE_FROM_HANDLE =
      clrTypes.getType("System.Type").GetMethod("GetTypeFromHandle", Array(clrTypes.getType("System.RuntimeTypeHandle")))

    val INT_PTR = clrTypes.getType("System.IntPtr")

    val JOBJECT = definitions.ObjectClass
    val JSTRING = definitions.StringClass

    val SystemConvert = clrTypes.getType("System.Convert")

    val objParam = Array(MOBJECT)

    val toBool:   MethodInfo = SystemConvert.GetMethod("ToBoolean", objParam) // see comment in emitUnbox
    val toSByte:  MethodInfo = SystemConvert.GetMethod("ToSByte",   objParam)
    val toShort:  MethodInfo = SystemConvert.GetMethod("ToInt16",   objParam)
    val toChar:   MethodInfo = SystemConvert.GetMethod("ToChar",    objParam)
    val toInt:    MethodInfo = SystemConvert.GetMethod("ToInt32",   objParam)
    val toLong:   MethodInfo = SystemConvert.GetMethod("ToInt64",   objParam)
    val toFloat:  MethodInfo = SystemConvert.GetMethod("ToSingle",  objParam)
    val toDouble: MethodInfo = SystemConvert.GetMethod("ToDouble",  objParam)

    //val boxedUnit: FieldInfo = msilType(definitions.BoxedUnitModule.info).GetField("UNIT")
    val boxedUnit: FieldInfo = fields(definitions.BoxedUnit_UNIT)

    // Scala attributes
    // symtab.Definitions -> object (singleton..)
    val SerializableAttr = definitions.SerializableAttr.tpe
    val CloneableAttr    = definitions.CloneableAttr.tpe
    val TransientAtt     = definitions.TransientAttr.tpe
    // remoting: the architectures are too different, no mapping (no portable code
    // possible)

    // java instance methods that are mapped to static methods in .net
    // these will need to be called with OpCodes.Call (not Callvirt)
    val dynToStatMapped = mutable.HashSet[Symbol]()

    initMappings()

    /** Create the mappings between java and .net classes and methods */
    private def initMappings() {
      mapType(definitions.AnyClass, MOBJECT)
      mapType(definitions.AnyRefClass, MOBJECT)
      //mapType(definitions.NullClass, clrTypes.getType("scala.AllRef$"))
      //mapType(definitions.NothingClass, clrTypes.getType("scala.All$"))
      // FIXME: for some reason the upper two lines map to null
      mapType(definitions.NullClass, EXCEPTION)
      mapType(definitions.NothingClass, EXCEPTION)

      mapType(definitions.BooleanClass, MBOOL)
      mapType(definitions.ByteClass, MBYTE)
      mapType(definitions.ShortClass, MSHORT)
      mapType(definitions.CharClass, MCHAR)
      mapType(definitions.IntClass, MINT)
      mapType(definitions.LongClass, MLONG)
      mapType(definitions.FloatClass, MFLOAT)
      mapType(definitions.DoubleClass, MDOUBLE)
    }

    var clasz: IClass = _
    var method: IMethod = _

    var massembly: AssemblyBuilder = _
    var mmodule: ModuleBuilder = _
    var mcode: ILGenerator = _

    var assemName: String = _
    var firstSourceName = ""
    var outDir: File = _
    var srcPath: File = _
    var moduleName: String = _

    def initAssembly() {

      assemName = settings.assemname.value

      if (assemName == "") {
        if (entryPoint != null) {
          assemName = msilName(entryPoint.enclClass)
          // remove the $ at the end (from module-name)
          assemName = assemName.substring(0, assemName.length() - 1)
        } else {
          // assuming filename of first source file
          assert(firstSourceName.endsWith(".scala"), firstSourceName)
          assemName = firstSourceName.substring(0, firstSourceName.length() - 6)
        }
      } else {
        if (assemName.endsWith(".msil"))
          assemName = assemName.substring(0, assemName.length()-5)
        if (assemName.endsWith(".il"))
          assemName = assemName.substring(0, assemName.length()-3)
        val f: File = new File(assemName)
        assemName = f.getName()
      }

      outDir = new File(settings.outdir.value)

      srcPath = new File(settings.sourcedir.value)

      val assemblyName = new AssemblyName()
      assemblyName.Name = assemName
      massembly = AssemblyBuilderFactory.DefineDynamicAssembly(assemblyName)

      moduleName = assemName // + (if (entryPoint == null) ".dll" else ".exe")
      // filename here: .dll or .exe (in both parameters), second: give absolute-path
      mmodule = massembly.DefineDynamicModule(moduleName,
                                              new File(outDir, moduleName).getAbsolutePath())
      assert (mmodule != null)
    }


    /**
     * Form of the custom Attribute parameter (Ecma-335.pdf)
     *      - p. 163 for CustomAttrib Form,
     *      - p. 164 for FixedArg Form (Array and Element) (if array or not is known!)
     *  !! least significant byte first if values longer than one byte !!
     *
     * 1: Prolog (unsigned int16, value 0x0001) -> symtab[0] = 0x01, symtab[1] = 0x00
     * 2: FixedArgs (directly the data, get number and types from related constructor)
     *  2.1: length of the array (unsigned int32, 4 bytes, least significant first)
     *  2.2: the byte array data
     * 3: NumNamed (unsigned int16, number of named fields and properties, 0x0000)
     */
    def addSymtabAttribute(sym: Symbol, tBuilder: TypeBuilder) {
      def addMarker() {
        val markerSymtab = new Array[Byte](4)
        markerSymtab(0) = 1.toByte
        tBuilder.SetCustomAttribute(SYMTAB_ATTRIBUTE_EMPTY_CONSTRUCTOR, markerSymtab)
      }

      // both conditions are needed (why exactly..?)
      if (tBuilder.Name.endsWith("$") || sym.isModuleClass) {
        addMarker()
      } else {
        currentRun.symData.get(sym) match {
          case Some(pickle) =>
            var size = pickle.writeIndex
            val symtab = new Array[Byte](size + 8)
            symtab(0) = 1.toByte
            for (i <- 2 until 6) {
              symtab(i) = (size & 0xff).toByte
              size = size >> 8
            }
            java.lang.System.arraycopy(pickle.bytes, 0, symtab, 6, pickle.writeIndex)

            tBuilder.SetCustomAttribute(SYMTAB_ATTRIBUTE_CONSTRUCTOR, symtab)

            currentRun.symData -= sym
            currentRun.symData -= sym.companionSymbol

          case _ =>
            addMarker()
        }
      }
    }

    /**
     * Mutates `member` adding CLR attributes (if any) based on sym.annotations.
     * Please notice that CLR custom modifiers are a different beast (see customModifiers below)
     * and thus shouldn't be added by this method.
     */
    def addAttributes(member: ICustomAttributeSetter, annotations: List[AnnotationInfo]) {
      val attributes = annotations.map(_.atp.typeSymbol).collect {
        case definitions.TransientAttr => null // TODO this is just an example
      }
      return // TODO: implement at some point
    }

    /**
     * What's a CLR custom modifier? Intro available as source comments in compiler.msil.CustomModifier.
     * It's basically a marker associated with a location (think of FieldInfo, ParameterInfo, and PropertyInfo)
     * and thus that marker (be it optional or required) becomes part of the signature of that location.
     * Some annotations will become CLR attributes (see addAttributes above), others custom modifiers (this method).
     */
    def customModifiers(annotations: List[AnnotationInfo]): Array[CustomModifier] = {
      annotations.map(_.atp.typeSymbol).collect {
        case definitions.VolatileAttr  => new CustomModifier(true, CustomModifier.VolatileMarker)
      } toArray
    }



    /*
      debuglog("creating annotations: " + annotations + " for member : " + member)
      for (annot@ AnnotationInfo(typ, annArgs, nvPairs) <- annotations ;
           if annot.isConstant)
           //!typ.typeSymbol.isJavaDefined
      {
//        assert(consts.length <= 1,
//               "too many constant arguments for annotations; "+consts.toString())

        // Problem / TODO having the symbol of the annotations type would be nicer
        // (i hope that type.typeSymbol is the same as the one in types2create)
        // AND: this will crash if the annotations Type is already compiled (-> not a typeBuilder)
        // when this is solved, types2create will be the same as icodes.classes, thus superfluous
        val annType: TypeBuilder = getType(typ.typeSymbol).asInstanceOf[TypeBuilder]
//        val annType: MsilType = getType(typ.typeSymbol)

        // Problem / TODO: i have no idea which constructor is used. This
        // information should be available in AnnotationInfo.
        annType.CreateType() // else, GetConstructors can't be used
        val constr: ConstructorInfo = annType.GetConstructors()(0)
        // prevent a second call of CreateType, only needed because there's no
        // other way than GetConstructors()(0) to get the constructor, if there's
        // no constructor symbol available.

        val args: Array[Byte] =
          getAttributeArgs(
            annArgs map (_.constant.get),
            (for((n,v) <- nvPairs) yield (n, v.constant.get)))
        member.SetCustomAttribute(constr, args)
      }
    } */

/*    def getAttributeArgs(consts: List[Constant], nvPairs: List[(Name, Constant)]): Array[Byte] = {
      val buf = ByteBuffer.allocate(2048) // FIXME: this may be not enough!
      buf.order(ByteOrder.LITTLE_ENDIAN)
      buf.putShort(1.toShort) // signature

      def emitSerString(str: String) = {
          // this is wrong, it has to be the length of the UTF-8 byte array, which
          // may be longer (see clr-book on page 302)
//          val length: Int = str.length
            val strBytes: Array[Byte] = try {
              str.getBytes("UTF-8")
            } catch {
              case _: Error => abort("could not get byte-array for string: " + str)
            }
            val length: Int = strBytes.length //this length is stored big-endian
            if (length < 128)
              buf.put(length.toByte)
            else if (length < (1<<14)) {
              buf.put(((length >> 8) | 0x80).toByte) // the bits 14 and 15 of length are '0'
              buf.put((length | 0xff).toByte)
            } else if (length < (1 << 29)) {
              buf.put(((length >> 24) | 0xc0).toByte)
              buf.put(((length >> 16) & 0xff).toByte)
              buf.put(((length >>  8) & 0xff).toByte)
              buf.put(((length      ) & 0xff).toByte)
            } else
              abort("string too long for attribute parameter: " + length)
            buf.put(strBytes)
      }

      def emitConst(const: Constant): Unit = const.tag match {
        case BooleanTag => buf.put((if (const.booleanValue) 1 else 0).toByte)
        case ByteTag => buf.put(const.byteValue)
        case ShortTag => buf.putShort(const.shortValue)
        case CharTag => buf.putChar(const.charValue)
        case IntTag => buf.putInt(const.intValue)
        case LongTag => buf.putLong(const.longValue)
        case FloatTag => buf.putFloat(const.floatValue)
        case DoubleTag => buf.putDouble(const.doubleValue)
        case StringTag =>
          val str: String = const.stringValue
          if (str == null) {
            buf.put(0xff.toByte)
          } else {
            emitSerString(str)
          }
        case ArrayTag =>
          val arr: Array[Constant] = const.arrayValue
          if (arr == null) {
            buf.putInt(0xffffffff)
          } else {
            buf.putInt(arr.length)
            arr.foreach(emitConst)
          }

        // TODO: other Tags: NoTag, UnitTag, ClazzTag, EnumTag, ArrayTag ???

        case _ => abort("could not handle attribute argument: " + const)
      }

      consts foreach emitConst
      buf.putShort(nvPairs.length.toShort)
      def emitNamedArg(nvPair: (Name, Constant)) {
        // the named argument is a property of the attribute (it can't be a field, since
        //  all fields in scala are private)
        buf.put(0x54.toByte)

        def emitType(c: Constant) = c.tag match { // type of the constant, Ecma-335.pdf, page 151
          case BooleanTag => buf.put(0x02.toByte)
          case ByteTag =>    buf.put(0x05.toByte)
          case ShortTag =>   buf.put(0x06.toByte)
          case CharTag =>    buf.put(0x07.toByte)
          case IntTag =>     buf.put(0x08.toByte)
          case LongTag =>    buf.put(0x0a.toByte)
          case FloatTag =>   buf.put(0x0c.toByte)
          case DoubleTag =>  buf.put(0x0d.toByte)
          case StringTag =>  buf.put(0x0e.toByte)

          // TODO: other Tags: NoTag, UnitTag, ClazzTag, EnumTag ???

          // ArrayTag falls in here
          case _ => abort("could not handle attribute argument: " + c)
        }

        val cnst: Constant = nvPair._2
        if (cnst.tag == ArrayTag) {
          buf.put(0x1d.toByte)
          emitType(cnst.arrayValue(0)) // FIXME: will crash if array length = 0
        } else if (cnst.tag == EnumTag) {
          buf.put(0x55.toByte)
          // TODO: put a SerString (don't know what exactly, names of the enums somehow..)
              } else {
          buf.put(0x51.toByte)
          emitType(cnst)
        }

        emitSerString(nvPair._1.toString)
        emitConst(nvPair._2)
      }

      val length = buf.position()
      buf.array().slice(0, length)
    } */

    def writeAssembly() {
      if (entryPoint != null) {
        assert(entryPoint.enclClass.isModuleClass, entryPoint.enclClass)
        val mainMethod = methods(entryPoint)
        val stringArrayTypes: Array[MsilType] = Array(MSTRING_ARRAY)
        val globalMain = mmodule.DefineGlobalMethod(
          "Main", MethodAttributes.Public | MethodAttributes.Static,
          MVOID, stringArrayTypes)
        globalMain.DefineParameter(0, ParameterAttributes.None, "args")
        massembly.SetEntryPoint(globalMain)
        val code = globalMain.GetILGenerator()
        val moduleField = getModuleInstanceField(entryPoint.enclClass)
        code.Emit(OpCodes.Ldsfld, moduleField)
        code.Emit(OpCodes.Ldarg_0)
        code.Emit(OpCodes.Callvirt, mainMethod)
        code.Emit(OpCodes.Ret)
      }
      createTypes()
      var outDirName: String = null
      try {
        if (settings.Ygenjavap.isDefault) { // we reuse the JVM-sounding setting because it's conceptually similar
          outDirName = outDir.getPath()
          massembly.Save(outDirName + "\\" + assemName + ".msil") /* use SingleFileILPrinterVisitor */
        } else {
          outDirName = srcPath.getPath()
          massembly.Save(settings.Ygenjavap.value, outDirName)  /* use MultipleFilesILPrinterVisitor */
        }
      } catch {
        case e:IOException => abort("Could not write to " + outDirName + ": " + e.getMessage())
      }
    }

    private def createTypes() {
      for (sym <- classes.keys) {
        val iclass   = classes(sym)
        val tBuilder = types(sym).asInstanceOf[TypeBuilder]

        debuglog("Calling CreatType for " + sym + ", " + tBuilder.toString)

        tBuilder.CreateType()
        tBuilder.setSourceFilepath(iclass.cunit.source.file.path)
      }
    }

    private[GenMSIL] def ilasmFileName(iclass: IClass) : String = {
      // method.sourceFile contains just the filename
      iclass.cunit.source.file.toString.replace("\\", "\\\\")
    }

    private[GenMSIL] def genClass(iclass: IClass) {
      val sym = iclass.symbol
      debuglog("Generating class " + sym + " flags: " + Flags.flagsToString(sym.flags))
      clasz = iclass

      val tBuilder = getType(sym).asInstanceOf[TypeBuilder]
      if (isCloneable(sym)) {
        // FIXME: why there's no nme.clone_ ?
        // "Clone": if the code is non-portable, "Clone" is defined, not "clone"
        // TODO: improve condition (should override AnyRef.clone)
        if (iclass.methods.forall(m => {
          !((m.symbol.name.toString != "clone" || m.symbol.name.toString != "Clone") &&
            m.symbol.tpe.paramTypes.length != 0)
        })) {
          debuglog("auto-generating cloneable method for " + sym)
          val attrs: Short = (MethodAttributes.Public | MethodAttributes.Virtual |
                              MethodAttributes.HideBySig).toShort
          val cloneMethod = tBuilder.DefineMethod("Clone", attrs, MOBJECT,
                                                  MsilType.EmptyTypes)
          val clCode = cloneMethod.GetILGenerator()
          clCode.Emit(OpCodes.Ldarg_0)
          clCode.Emit(OpCodes.Call, MEMBERWISE_CLONE)
          clCode.Emit(OpCodes.Ret)
        }
      }

      val line = sym.pos.line
      tBuilder.setPosition(line, ilasmFileName(iclass))

      if (isTopLevelModule(sym)) {
        if (sym.companionClass == NoSymbol)
          generateMirrorClass(sym)
        else
          log("No mirror class for module with linked class: " +
              sym.fullName)
      }

      addSymtabAttribute(sym, tBuilder)
      addAttributes(tBuilder, sym.annotations)

      if (iclass.symbol != definitions.ArrayClass)
        iclass.methods foreach genMethod

    } //genClass


    private def genMethod(m: IMethod) {
      debuglog("Generating method " + m.symbol + " flags: " + Flags.flagsToString(m.symbol.flags) +
            " owner: " + m.symbol.owner)
      method = m
      localBuilders.clear
      computeLocalVarsIndex(m)

      if (m.symbol.isClassConstructor) {
        mcode = constructors(m.symbol).asInstanceOf[ConstructorBuilder].GetILGenerator()
      } else {
        val mBuilder = methods(m.symbol).asInstanceOf[MethodBuilder]
        if (!mBuilder.IsAbstract())
          try {
            mcode = mBuilder.GetILGenerator()
          } catch {
            case e: Exception =>
              java.lang.System.out.println("m.symbol       = " + Flags.flagsToString(m.symbol.flags) + " " + m.symbol)
              java.lang.System.out.println("m.symbol.owner = " + Flags.flagsToString(m.symbol.owner.flags) + " " + m.symbol.owner)
              java.lang.System.out.println("mBuilder       = " + mBuilder)
              java.lang.System.out.println("mBuilder.DeclaringType = " +
                                 TypeAttributes.toString(mBuilder.DeclaringType.Attributes) +
                                 "::" + mBuilder.DeclaringType)
              throw e
          }
          else
            mcode = null
      }

      if (mcode != null) {
        for (local <- m.locals ; if !(m.params contains local)) {
          debuglog("add local var: " + local + ", of kind " + local.kind)
          val t: MsilType = msilType(local.kind)
          val localBuilder = mcode.DeclareLocal(t)
          localBuilder.SetLocalSymInfo(msilName(local.sym))
          localBuilders(local) = localBuilder
        }
        genCode(m)
      }

    }

    /** Special linearizer for methods with at least one exception handler. This
     *  linearizer brings all basic blocks in the right order so that nested
     *  try-catch and try-finally blocks can be emitted.
     */
    val msilLinearizer = new MSILLinearizer()

    val labels = mutable.HashMap[BasicBlock, Label]()

    /* when emitting .line, it's enough to include the full filename just once per method, thus reducing filesize.
     * this scheme relies on the fact that the entry block is emitted first. */
    var dbFilenameSeen = false

    def genCode(m: IMethod) {

      def makeLabels(blocks: List[BasicBlock]) = {
        debuglog("Making labels for: " + method)
        for (bb <- blocks) labels(bb) = mcode.DefineLabel()
      }

      labels.clear

      var linearization = if(!m.exh.isEmpty) msilLinearizer.linearize(m)
                          else linearizer.linearize(m)

      if (!m.exh.isEmpty)
        linearization = computeExceptionMaps(linearization, m)

      makeLabels(linearization)

      // debug val blocksInM = m.code.blocks.toList.sortBy(bb => bb.label)
      // debug val blocksInL = linearization.sortBy(bb => bb.label)
      // debug val MButNotL  = (blocksInM.toSet) diff (blocksInL.toSet) // if non-empty, a jump to B fails to find a label for B (case CJUMP, case CZJUMP)
      // debug if(!MButNotL.isEmpty) { }

      dbFilenameSeen = false
      genBlocks(linearization)

      // RETURN inside exception blocks are replaced by Leave. The target of the
      // leave is a `Ret` outside any exception block (generated here).
      if (handlerReturnMethod == m) {
        mcode.MarkLabel(handlerReturnLabel)
        if (handlerReturnKind != UNIT)
          mcode.Emit(OpCodes.Ldloc, handlerReturnLocal)
        mcode.Emit(OpCodes.Ret)
      }

      beginExBlock.clear()
      beginCatchBlock.clear()
      endExBlock.clear()
      endFinallyLabels.clear()
    }

    def genBlocks(blocks: List[BasicBlock], previous: BasicBlock = null) {
      blocks match {
        case Nil => ()
        case x :: Nil => genBlock(x, prev = previous, next = null)
        case x :: y :: ys => genBlock(x, prev = previous, next = y); genBlocks(y :: ys, previous = x)
      }
    }

    // the try blocks starting at a certain BasicBlock
    val beginExBlock = mutable.HashMap[BasicBlock, List[ExceptionHandler]]()

    // the catch blocks starting / endling at a certain BasicBlock
    val beginCatchBlock = mutable.HashMap[BasicBlock, ExceptionHandler]()
    val endExBlock = mutable.HashMap[BasicBlock, List[ExceptionHandler]]()

    /** When emitting the code (genBlock), the number of currently active try / catch
     *  blocks. When seeing a `RETURN` inside a try / catch, we need to
     *   - store the result in a local (if it's not UNIT)
     *   - emit `Leave handlerReturnLabel` instead of the Return
     *   - emit code at the end: load the local and return its value
     */
    var currentHandlers = new mutable.Stack[ExceptionHandler]
    // The IMethod the Local/Label/Kind below belong to
    var handlerReturnMethod: IMethod = _
    // Stores the result when returning inside an exception block
    var handlerReturnLocal: LocalBuilder = _
    // Label for a return instruction outside any exception block
    var handlerReturnLabel: Label = _
    // The result kind.
    var handlerReturnKind: TypeKind = _
    def returnFromHandler(kind: TypeKind): (LocalBuilder, Label) = {
      if (handlerReturnMethod != method) {
        handlerReturnMethod = method
        if (kind != UNIT) {
          handlerReturnLocal = mcode.DeclareLocal(msilType(kind))
          handlerReturnLocal.SetLocalSymInfo("$handlerReturn")
        }
        handlerReturnLabel = mcode.DefineLabel()
        handlerReturnKind = kind
      }
      (handlerReturnLocal, handlerReturnLabel)
    }

    /** For try/catch nested inside a finally, we can't use `Leave OutsideFinally`, the
     *  Leave target has to be inside the finally (and it has to be the `endfinally` instruction).
     *  So for every finalizer, we have a label which marks the place of the `endfinally`,
     *  nested try/catch blocks will leave there.
     */
    val endFinallyLabels = mutable.HashMap[ExceptionHandler, Label]()

    /** Computes which blocks are the beginning / end of a try or catch block */
    private def computeExceptionMaps(blocks: List[BasicBlock], m: IMethod): List[BasicBlock] = {
      val visitedBlocks = new mutable.HashSet[BasicBlock]()

      // handlers which have not been introduced so far
      var openHandlers = m.exh


      /** Example
       *   try {
       *     try {
       *         // *1*
       *     } catch {
       *       case h1 =>
       *     }
       *   } catch {
       *     case h2 =>
       *     case h3 =>
       *       try {
       *
       *       } catch {
       *         case h4 =>  // *2*
       *         case h5 =>
       *       }
       *   }
       */

      // Stack of nested try blocks. Each bloc has a List of ExceptionHandler (multiple
      // catch statements). Example *1*: Stack(List(h2, h3), List(h1))
      val currentTryHandlers = new mutable.Stack[List[ExceptionHandler]]()

      // Stack of nested catch blocks. The head of the list is the current catch block. The
      // tail is all following catch blocks. Example *2*: Stack(List(h3), List(h4, h5))
      val currentCatchHandlers = new mutable.Stack[List[ExceptionHandler]]()

      for (b <- blocks) {

        // are we past the current catch blocks?
        def endHandlers(): List[ExceptionHandler] = {
          var res: List[ExceptionHandler] = Nil
          if (!currentCatchHandlers.isEmpty) {
            val handler = currentCatchHandlers.top.head
            if (!handler.blocks.contains(b)) {
              // all blocks of the handler are either visited, or not part of the linearization (i.e. dead)
              assert(handler.blocks.forall(b => visitedBlocks.contains(b) || !blocks.contains(b)),
                     "Bad linearization of basic blocks inside catch. Found block not part of the handler\n"+
                     b.fullString +"\nwhile in catch-part of\n"+ handler)

              val rest = currentCatchHandlers.pop.tail
              if (rest.isEmpty) {
                // all catch blocks of that exception handler are covered
                res = handler :: endHandlers()
              } else {
                // there are more catch blocks for that try (handlers covering the same)
                currentCatchHandlers.push(rest)
                beginCatchBlock(b) = rest.head
              }
            }
          }
          res
        }
        val end = endHandlers()
        if (!end.isEmpty) endExBlock(b) = end

        // are we past the current try block?
        if (!currentTryHandlers.isEmpty) {
          val handler = currentTryHandlers.top.head
          if (!handler.covers(b)) {
            // all of the covered blocks are visited, or not part of the linearization
            assert(handler.covered.forall(b => visitedBlocks.contains(b) || !blocks.contains(b)),
                   "Bad linearization of basic blocks inside try. Found non-covered block\n"+
                   b.fullString +"\nwhile in try-part of\n"+ handler)

            assert(handler.startBlock == b,
                   "Bad linearization of basic blocks. The entry block of a catch does not directly follow the try\n"+
                   b.fullString +"\n"+ handler)

            val handlers = currentTryHandlers.pop
            currentCatchHandlers.push(handlers)
            beginCatchBlock(b) = handler
          }
        }

        // are there try blocks starting at b?
        val (newHandlers, stillOpen) = openHandlers.partition(_.covers(b))
        openHandlers = stillOpen

        val newHandlersBySize = newHandlers.groupBy(_.covered.size)
        // big handlers first, smaller ones are nested inside the try of the big one
        // (checked by the assertions below)
        val sizes = newHandlersBySize.keys.toList.sortWith(_ > _)

        val beginHandlers = new mutable.ListBuffer[ExceptionHandler]
        for (s <- sizes) {
          val sHandlers = newHandlersBySize(s)
          for (h <- sHandlers) {
            assert(h.covered == sHandlers.head.covered,
                   "bad nesting of exception handlers. same size, but not covering same blocks\n"+
                   h +"\n"+ sHandlers.head)
            assert(h.resultKind == sHandlers.head.resultKind,
                   "bad nesting of exception handlers. same size, but the same resultKind\n"+
                   h +"\n"+ sHandlers.head)
          }
          for (bigger <- beginHandlers; h <- sHandlers) {
            assert(h.covered.subsetOf(bigger.covered),
                   "bad nesting of exception handlers. try blocks of smaller handler are not nested in bigger one.\n"+
                   h +"\n"+ bigger)
            assert(h.blocks.toSet.subsetOf(bigger.covered),
                   "bad nesting of exception handlers. catch blocks of smaller handler are not nested in bigger one.\n"+
                   h +"\n"+ bigger)
          }
          beginHandlers += sHandlers.head
          currentTryHandlers.push(sHandlers)
        }
        beginExBlock(b) = beginHandlers.toList
        visitedBlocks += b
      }

      // if there handlers left (i.e. handlers covering nothing, or a
      // non-existent (dead) block), remove their catch-blocks.
      val liveBlocks = if (openHandlers.isEmpty) blocks else {
        blocks.filter(b => openHandlers.forall(h => !h.blocks.contains(b)))
      }

      /** There might be open handlers, but no more blocks. happens when try/catch end
       *  with `throw` or `return`
       *     def foo() { try { .. throw } catch { _ => .. throw } }
       *
       *  In this case we need some code after the catch block for the auto-generated
       *  `leave` instruction. So we're adding a (dead) `throw new Exception`.
       */
      val rest = currentCatchHandlers.map(handlers => {
        assert(handlers.length == 1, handlers)
        handlers.head
      }).toList

      if (rest.isEmpty) {
        liveBlocks
      } else {
        val b = m.code.newBlock
        b.emit(Seq(
          NEW(REFERENCE(definitions.ThrowableClass)),
          DUP(REFERENCE(definitions.ObjectClass)),
          CALL_METHOD(definitions.ThrowableClass.primaryConstructor, Static(true)),
          THROW(definitions.ThrowableClass)
        ))
        b.close
        endExBlock(b) = rest
        liveBlocks ::: List(b)
      }
    }

    /**
     *  @param block the BasicBlock to emit code for
     *  @param next  the following BasicBlock, `null` if `block` is the last one
     */
    def genBlock(block: BasicBlock, prev: BasicBlock, next: BasicBlock) {

      def loadLocalOrAddress(local: Local, msg : String , loadAddr : Boolean) {
        debuglog(msg + " for " + local)
        val isArg = local.arg
        val i = local.index
        if (isArg)
          loadArg(mcode, loadAddr)(i)
        else
          loadLocal(i, local, mcode, loadAddr)
      }

      def loadFieldOrAddress(field: Symbol, isStatic: Boolean, msg: String, loadAddr : Boolean) {
        debuglog(msg + " with owner: " + field.owner +
              " flags: " + Flags.flagsToString(field.owner.flags))
        var fieldInfo = fields.get(field) match {
          case Some(fInfo) => fInfo
          case None =>
            val fInfo = getType(field.owner).GetField(msilName(field))
            fields(field) = fInfo
            fInfo
        }
        if (fieldInfo.IsVolatile) {
          mcode.Emit(OpCodes.Volatile)
        }
        if (!fieldInfo.IsLiteral) {
          if (loadAddr) {
            mcode.Emit(if (isStatic) OpCodes.Ldsflda else OpCodes.Ldflda, fieldInfo)
          } else {
            mcode.Emit(if (isStatic) OpCodes.Ldsfld else OpCodes.Ldfld, fieldInfo)
          }
        } else {
          assert(!loadAddr, "can't take AddressOf a literal field (not even with readonly. prefix) because no memory was allocated to such field ...")
          // TODO the above can be overcome by loading the value, boxing, and finally unboxing. An address to a copy of the raw value will be on the stack.
         /*  We perform `field inlining' as required by CLR.
          *  Emit as for a CONSTANT ICode stmt, with the twist that the constant value is available
          *  as a java.lang.Object and its .NET type allows constant initialization in CLR, i.e. that type
          *  is one of I1, I2, I4, I8, R4, R8, CHAR, BOOLEAN, STRING, or CLASS (in this last case,
          *  only accepting nullref as value). See Table 9-1 in Lidin's book on ILAsm. */
          val value = fieldInfo.getValue()
          if (value == null) {
            mcode.Emit(OpCodes.Ldnull)
          } else {
            val typ = if (fieldInfo.FieldType.IsEnum) fieldInfo.FieldType.getUnderlyingType
                      else fieldInfo.FieldType
            if (typ == clrTypes.STRING) {
              mcode.Emit(OpCodes.Ldstr, value.asInstanceOf[String])
            } else if (typ == clrTypes.BOOLEAN) {
                mcode.Emit(if (value.asInstanceOf[Boolean]) OpCodes.Ldc_I4_1
                           else OpCodes.Ldc_I4_0)
            } else if (typ == clrTypes.BYTE || typ == clrTypes.UBYTE) {
              loadI4(value.asInstanceOf[Byte], mcode)
            } else if (typ == clrTypes.SHORT || typ == clrTypes.USHORT) {
              loadI4(value.asInstanceOf[Int], mcode)
            } else if (typ == clrTypes.CHAR) {
              loadI4(value.asInstanceOf[Char], mcode)
            } else if (typ == clrTypes.INT || typ == clrTypes.UINT) {
              loadI4(value.asInstanceOf[Int], mcode)
            } else if (typ == clrTypes.LONG || typ == clrTypes.ULONG) {
              mcode.Emit(OpCodes.Ldc_I8, value.asInstanceOf[Long])
            } else if (typ == clrTypes.FLOAT) {
              mcode.Emit(OpCodes.Ldc_R4, value.asInstanceOf[Float])
            } else if (typ == clrTypes.DOUBLE) {
              mcode.Emit(OpCodes.Ldc_R8, value.asInstanceOf[Double])
            } else {
              /* TODO one more case is described in Partition II, 16.2: bytearray(...) */
              abort("Unknown type for static literal field: " + fieldInfo)
            }
          }
        }
      }

      /** Creating objects works differently on .NET. On the JVM
       *  - NEW(type) => reference on Stack
       *  - DUP, load arguments, CALL_METHOD(constructor)
       *
       * On .NET, the NEW and DUP are ignored, but we emit a special method call
       *  - load arguments
       *  - NewObj(constructor) => reference on stack
       *
       * This variable tells whether the previous instruction was a NEW,
       * we expect a DUP which is not emitted. */
      var previousWasNEW = false

      var lastLineNr: Int = 0
      var lastPos: Position = NoPosition


      // EndExceptionBlock must happen before MarkLabel because it adds the
      // Leave instruction. Otherwise, labels(block) points to the Leave
      // (inside the catch) instead of the instruction afterwards.
      for (handlers <- endExBlock.get(block); exh <- handlers) {
        currentHandlers.pop()
        for (l <- endFinallyLabels.get(exh))
          mcode.MarkLabel(l)
        mcode.EndExceptionBlock()
      }

      mcode.MarkLabel(labels(block))
      debuglog("Generating code for block: " + block)

      for (handler <- beginCatchBlock.get(block)) {
        if (!currentHandlers.isEmpty && currentHandlers.top.covered == handler.covered) {
          currentHandlers.pop()
          currentHandlers.push(handler)
        }
        if (handler.cls == NoSymbol) {
          // `finally` blocks are represented the same as `catch`, but with no catch-type
          mcode.BeginFinallyBlock()
        } else {
          val t = getType(handler.cls)
          mcode.BeginCatchBlock(t)
        }
      }
      for (handlers <- beginExBlock.get(block); exh <- handlers) {
        currentHandlers.push(exh)
        mcode.BeginExceptionBlock()
      }

      for (instr <- block) {
        try {
          val currentLineNr = instr.pos.line
          val skip = if(instr.pos.isRange) instr.pos.sameRange(lastPos) else (currentLineNr == lastLineNr);
          if(!skip || !dbFilenameSeen) {
            val fileName = if(dbFilenameSeen) "" else {dbFilenameSeen = true; ilasmFileName(clasz)};
            if(instr.pos.isRange) {
              val startLine = instr.pos.focusStart.line
              val endLine   = instr.pos.focusEnd.line
              val startCol  = instr.pos.focusStart.column
              val endCol    = instr.pos.focusEnd.column
              mcode.setPosition(startLine, endLine, startCol, endCol, fileName)
            } else {
              mcode.setPosition(instr.pos.line, fileName)
            }
            lastLineNr = currentLineNr
            lastPos = instr.pos
          }
        } catch { case _: UnsupportedOperationException => () }

        if (previousWasNEW)
          assert(instr.isInstanceOf[DUP], block)

        instr match {
          case THIS(clasz) =>
            mcode.Emit(OpCodes.Ldarg_0)

          case CONSTANT(const) =>
            const.tag match {
              case UnitTag    => ()
              case BooleanTag => mcode.Emit(if (const.booleanValue) OpCodes.Ldc_I4_1
                                            else OpCodes.Ldc_I4_0)
              case ByteTag    => loadI4(const.byteValue, mcode)
              case ShortTag   => loadI4(const.shortValue, mcode)
              case CharTag    => loadI4(const.charValue, mcode)
              case IntTag     => loadI4(const.intValue, mcode)
              case LongTag    => mcode.Emit(OpCodes.Ldc_I8, const.longValue)
              case FloatTag   => mcode.Emit(OpCodes.Ldc_R4, const.floatValue)
              case DoubleTag  => mcode.Emit(OpCodes.Ldc_R8, const.doubleValue)
              case StringTag  => mcode.Emit(OpCodes.Ldstr, const.stringValue)
              case NullTag    => mcode.Emit(OpCodes.Ldnull)
              case ClazzTag   =>
                mcode.Emit(OpCodes.Ldtoken, msilType(const.typeValue))
                mcode.Emit(OpCodes.Call, TYPE_FROM_HANDLE)
              case _          => abort("Unknown constant value: " + const)
            }

          case LOAD_ARRAY_ITEM(kind) =>
            (kind: @unchecked) match {
              case BOOL           => mcode.Emit(OpCodes.Ldelem_I1)
              case BYTE           => mcode.Emit(OpCodes.Ldelem_I1) // I1 for System.SByte, i.e. a scala.Byte
              case SHORT          => mcode.Emit(OpCodes.Ldelem_I2)
              case CHAR           => mcode.Emit(OpCodes.Ldelem_U2)
              case INT            => mcode.Emit(OpCodes.Ldelem_I4)
              case LONG           => mcode.Emit(OpCodes.Ldelem_I8)
              case FLOAT          => mcode.Emit(OpCodes.Ldelem_R4)
              case DOUBLE         => mcode.Emit(OpCodes.Ldelem_R8)
              case REFERENCE(cls) => mcode.Emit(OpCodes.Ldelem_Ref)
              case ARRAY(elem)    => mcode.Emit(OpCodes.Ldelem_Ref)

              // case UNIT is not possible: an Array[Unit] will be an
              //  Array[scala.runtime.BoxedUnit] (-> case REFERENCE)
            }

          case LOAD_LOCAL(local) => loadLocalOrAddress(local, "load_local", false)

          case CIL_LOAD_LOCAL_ADDRESS(local) => loadLocalOrAddress(local, "cil_load_local_address", true)

          case LOAD_FIELD(field, isStatic) => loadFieldOrAddress(field, isStatic, "load_field", false)

          case CIL_LOAD_FIELD_ADDRESS(field, isStatic) => loadFieldOrAddress(field, isStatic, "cil_load_field_address", true)

          case CIL_LOAD_ARRAY_ITEM_ADDRESS(kind) => mcode.Emit(OpCodes.Ldelema, msilType(kind))

          case CIL_NEWOBJ(msym) =>
            assert(msym.isClassConstructor)
            val constructorInfo: ConstructorInfo = getConstructor(msym)
            mcode.Emit(OpCodes.Newobj, constructorInfo)

          case LOAD_MODULE(module) =>
            debuglog("Generating LOAD_MODULE for: " + showsym(module))
            mcode.Emit(OpCodes.Ldsfld, getModuleInstanceField(module))

          case STORE_ARRAY_ITEM(kind) =>
            (kind: @unchecked) match {
              case BOOL           => mcode.Emit(OpCodes.Stelem_I1)
              case BYTE           => mcode.Emit(OpCodes.Stelem_I1)
              case SHORT          => mcode.Emit(OpCodes.Stelem_I2)
              case CHAR           => mcode.Emit(OpCodes.Stelem_I2)
              case INT            => mcode.Emit(OpCodes.Stelem_I4)
              case LONG           => mcode.Emit(OpCodes.Stelem_I8)
              case FLOAT          => mcode.Emit(OpCodes.Stelem_R4)
              case DOUBLE         => mcode.Emit(OpCodes.Stelem_R8)
              case REFERENCE(cls) => mcode.Emit(OpCodes.Stelem_Ref)
              case ARRAY(elem)    => mcode.Emit(OpCodes.Stelem_Ref) // @TODO: test this! (occurs when calling a Array[Object]* vararg param method)

              // case UNIT not possible (see comment at LOAD_ARRAY_ITEM)
            }

          case STORE_LOCAL(local) =>
            val isArg = local.arg
            val i = local.index
            debuglog("store_local for " + local + ", index " + i)

            // there are some locals defined by the compiler that
            // are isArg and are need to be stored.
            if (isArg) {
              if (i >= -128 && i <= 127)
                mcode.Emit(OpCodes.Starg_S, i)
              else
                mcode.Emit(OpCodes.Starg, i)
            } else {
              i match {
                case 0 => mcode.Emit(OpCodes.Stloc_0)
                case 1 => mcode.Emit(OpCodes.Stloc_1)
                case 2 => mcode.Emit(OpCodes.Stloc_2)
                case 3 => mcode.Emit(OpCodes.Stloc_3)
                case _      =>
                  if (i >= -128 && i <= 127)
                    mcode.Emit(OpCodes.Stloc_S, localBuilders(local))
                  else
                    mcode.Emit(OpCodes.Stloc, localBuilders(local))
              }
            }

          case STORE_THIS(_) =>
            // this only works for impl classes because the self parameter comes first
            // in the method signature. If that changes, this code has to be revisited.
            mcode.Emit(OpCodes.Starg_S, 0)

          case STORE_FIELD(field, isStatic) =>
            val fieldInfo = fields.get(field) match {
              case Some(fInfo) => fInfo
              case None =>
                val fInfo = getType(field.owner).GetField(msilName(field))
                fields(field) = fInfo
                fInfo
            }
            mcode.Emit(if (isStatic) OpCodes.Stsfld else OpCodes.Stfld, fieldInfo)

          case CALL_PRIMITIVE(primitive) =>
            genPrimitive(primitive, instr.pos)

          case CALL_METHOD(msym, style) =>
            if (msym.isClassConstructor) {
              val constructorInfo: ConstructorInfo = getConstructor(msym)
              (style: @unchecked) match {
                // normal constructor calls are Static..
                case Static(_) =>
                  if (method.symbol.isClassConstructor && method.symbol.owner == msym.owner)
                    // we're generating a constructor (method: IMethod is a constructor), and we're
                    // calling another constructor of the same class.

                    // @LUC TODO: this can probably break, namely when having: class A { def this() { new A() } }
                    // instead, we should instruct the CALL_METHOD with additional information, know whether it's
                    // an instance creation constructor call or not.
                    mcode.Emit(OpCodes.Call, constructorInfo)
                  else
                    mcode.Emit(OpCodes.Newobj, constructorInfo)
                case SuperCall(_) =>
                  mcode.Emit(OpCodes.Call, constructorInfo)
                  if (isStaticModule(clasz.symbol) &&
                      notInitializedModules.contains(clasz.symbol) &&
                      method.symbol.isClassConstructor)
                    {
                      notInitializedModules -= clasz.symbol
                      mcode.Emit(OpCodes.Ldarg_0)
                      mcode.Emit(OpCodes.Stsfld, getModuleInstanceField(clasz.symbol))
                    }
              }

            } else {

              var doEmit = true
              getTypeOpt(msym.owner) match {
                case Some(typ) if (typ.IsEnum) => {
                  def negBool() = {
                    mcode.Emit(OpCodes.Ldc_I4_0)
                    mcode.Emit(OpCodes.Ceq)
                  }
                  doEmit = false
                  val name = msym.name
                  if (name eq nme.EQ)       { mcode.Emit(OpCodes.Ceq) }
                  else if (name eq nme.NE)  { mcode.Emit(OpCodes.Ceq); negBool }
                  else if (name eq nme.LT)  { mcode.Emit(OpCodes.Clt) }
                  else if (name eq nme.LE)  { mcode.Emit(OpCodes.Cgt); negBool }
                  else if (name eq nme.GT)  { mcode.Emit(OpCodes.Cgt) }
                  else if (name eq nme.GE)  { mcode.Emit(OpCodes.Clt); negBool }
                  else if (name eq nme.OR)  { mcode.Emit(OpCodes.Or) }
                  else if (name eq nme.AND) { mcode.Emit(OpCodes.And) }
                  else if (name eq nme.XOR) { mcode.Emit(OpCodes.Xor) }
                  else
                    doEmit = true
                }
                case _ => ()
              }

              // method: implicit view(FunctionX[PType0, PType1, ...,PTypeN, ResType]):DelegateType
              val (isDelegateView, paramType, resType) = beforeTyper {
                msym.tpe match {
                  case MethodType(params, resultType)
                  if (params.length == 1 && msym.name == nme.view_) =>
                    val paramType = params(0).tpe
                    val isDel = definitions.isCorrespondingDelegate(resultType, paramType)
                    (isDel, paramType, resultType)
                  case _ => (false, null, null)
                }
              }
              if (doEmit && isDelegateView) {
                doEmit = false
                createDelegateCaller(paramType, resType)
              }

              if (doEmit &&
                  (msym.name == nme.PLUS || msym.name == nme.MINUS)
                  && clrTypes.isDelegateType(msilType(msym.owner.tpe)))
                {
                doEmit = false
                val methodInfo: MethodInfo = getMethod(msym)
                // call it as a static method, even if the compiler (symbol) thinks it's virtual
                mcode.Emit(OpCodes.Call, methodInfo)
                mcode.Emit(OpCodes.Castclass, msilType(msym.owner.tpe))
              }

              if (doEmit && definitions.Delegate_scalaCallers.contains(msym)) {
                doEmit = false
                val methodSym: Symbol = definitions.Delegate_scalaCallerTargets(msym)
                val delegateType: Type = msym.tpe match {
                  case MethodType(_, retType) => retType
                  case _ => abort("not a method type: " + msym.tpe)
                }
                val methodInfo: MethodInfo = getMethod(methodSym)
                val delegCtor = msilType(delegateType).GetConstructor(Array(MOBJECT, INT_PTR))
                if (methodSym.isStatic) {
                  mcode.Emit(OpCodes.Ldftn, methodInfo)
                } else {
                  mcode.Emit(OpCodes.Dup)
                  mcode.Emit(OpCodes.Ldvirtftn, methodInfo)
                }
                mcode.Emit(OpCodes.Newobj, delegCtor)
              }

              if (doEmit) {
                val methodInfo: MethodInfo = getMethod(msym)
                (style: @unchecked) match {
                  case SuperCall(_) =>
                    mcode.Emit(OpCodes.Call, methodInfo)
                  case Dynamic =>
                    // methodInfo.DeclaringType is null for global methods
                    val isValuetypeMethod = (methodInfo.DeclaringType ne null) && (methodInfo.DeclaringType.IsValueType)
                    val isValuetypeVirtualMethod = isValuetypeMethod && (methodInfo.IsVirtual)
                    if (dynToStatMapped(msym)) {
                      mcode.Emit(OpCodes.Call, methodInfo)
                    } else if (isValuetypeVirtualMethod) {
                      mcode.Emit(OpCodes.Constrained, methodInfo.DeclaringType)
                      mcode.Emit(OpCodes.Callvirt, methodInfo)
                    } else if (isValuetypeMethod) {
                      // otherwise error "Callvirt on a value type method" ensues
                      mcode.Emit(OpCodes.Call, methodInfo)
                    } else {
                      mcode.Emit(OpCodes.Callvirt, methodInfo)
                    }
                  case Static(_) =>
                    if(methodInfo.IsVirtual && !mcode.Ldarg0WasJustEmitted) {
                      mcode.Emit(OpCodes.Callvirt, methodInfo)
                    } else mcode.Emit(OpCodes.Call, methodInfo)
              }
            }
            }

          case BOX(boxType) =>
            emitBox(mcode, boxType)

          case UNBOX(boxType) =>
            emitUnbox(mcode, boxType)

          case CIL_UNBOX(boxType) =>
            mcode.Emit(OpCodes.Unbox, msilType(boxType))

          case CIL_INITOBJ(valueType) =>
            mcode.Emit(OpCodes.Initobj, msilType(valueType))

          case NEW(REFERENCE(cls)) =>
            // the next instruction must be a DUP, see comment on `var previousWasNEW`
            previousWasNEW = true

          // works also for arrays and reference-types
          case CREATE_ARRAY(elem, dims) =>
            // TODO: handle multi dimensional arrays
            assert(dims == 1, "Can't handle multi dimensional arrays")
            mcode.Emit(OpCodes.Newarr, msilType(elem))

          // works for arrays and reference-types
          case IS_INSTANCE(tpe) =>
            mcode.Emit(OpCodes.Isinst, msilType(tpe))
            mcode.Emit(OpCodes.Ldnull)
            mcode.Emit(OpCodes.Ceq)
            mcode.Emit(OpCodes.Ldc_I4_0)
            mcode.Emit(OpCodes.Ceq)

          // works for arrays and reference-types
          // part from the scala reference: "S <: T does not imply
          //  Array[S] <: Array[T] in Scala. However, it is possible
          //  to cast an array of S to an array of T if such a cast
          //  is permitted in the host environment."
          case CHECK_CAST(tpknd) =>
            val tMSIL = msilType(tpknd)
              mcode.Emit(OpCodes.Castclass, tMSIL)

          // no SWITCH is generated when there's
          //  - a default case ("case _ => ...") in the matching expr
          //  - OR is used ("case 1 | 2 => ...")
          case SWITCH(tags, branches) =>
            // tags is List[List[Int]]; a list of integers for every label.
            //    if the int on stack is 4, and 4 is in the second list => jump
            //    to second label
            // branches is List[BasicBlock]
            //    the labels to jump to (the last one is the default one)

            val switchLocal = mcode.DeclareLocal(MINT)
            // several switch variables will appear with the same name in the
            //  assembly code, but this makes no truble
            switchLocal.SetLocalSymInfo("$switch_var")

            mcode.Emit(OpCodes.Stloc, switchLocal)
            var i = 0
            for (l <- tags) {
              var targetLabel = labels(branches(i))
              for (i <- l) {
                mcode.Emit(OpCodes.Ldloc, switchLocal)
                loadI4(i, mcode)
                mcode.Emit(OpCodes.Beq, targetLabel)
              }
              i += 1
            }
            val defaultTarget = labels(branches(i))
            if (next != branches(i))
              mcode.Emit(OpCodes.Br, defaultTarget)

          case JUMP(whereto) =>
            val (leaveHandler, leaveFinally, lfTarget) = leavesHandler(block, whereto)
            if (leaveHandler) {
              if (leaveFinally) {
                if (lfTarget.isDefined) mcode.Emit(OpCodes.Leave, lfTarget.get)
                else mcode.Emit(OpCodes.Endfinally)
              } else
                mcode.Emit(OpCodes.Leave, labels(whereto))
            } else if (next != whereto)
              mcode.Emit(OpCodes.Br, labels(whereto))

          case CJUMP(success, failure, cond, kind) =>
            // cond is TestOp (see Primitives.scala), and can take
            // values EQ, NE, LT, GE LE, GT
            // kind is TypeKind
            val isFloat = kind == FLOAT || kind == DOUBLE
            val emit = (c: TestOp, l: Label) => emitBr(c, l, isFloat)
            emitCondBr(block, cond, success, failure, next, emit)

          case CZJUMP(success, failure, cond, kind) =>
            emitCondBr(block, cond, success, failure, next, emitBrBool(_, _))

          case RETURN(kind) =>
            if (currentHandlers.isEmpty)
              mcode.Emit(OpCodes.Ret)
            else {
              val (local, label) = returnFromHandler(kind)
              if (kind != UNIT)
                mcode.Emit(OpCodes.Stloc, local)
              mcode.Emit(OpCodes.Leave, label)
            }

          case THROW(_) =>
            mcode.Emit(OpCodes.Throw)

          case DROP(kind) =>
            mcode.Emit(OpCodes.Pop)

          case DUP(kind) =>
            // see comment on `var previousWasNEW`
            if (!previousWasNEW)
              mcode.Emit(OpCodes.Dup)
            else
              previousWasNEW = false

          case MONITOR_ENTER() =>
            mcode.Emit(OpCodes.Call, MMONITOR_ENTER)

          case MONITOR_EXIT() =>
            mcode.Emit(OpCodes.Call, MMONITOR_EXIT)

          case SCOPE_ENTER(_) | SCOPE_EXIT(_) | LOAD_EXCEPTION(_) =>
            ()
        }

      } // end for (instr <- b) { .. }
    } // end genBlock

    def genPrimitive(primitive: Primitive, pos: Position) {
      primitive match {
        case Negation(kind) =>
          kind match {
            // CHECK: is ist possible to get this for BOOL? in this case, verify.
            case BOOL | BYTE | CHAR | SHORT | INT | LONG | FLOAT | DOUBLE =>
              mcode.Emit(OpCodes.Neg)

            case _ => abort("Impossible to negate a " + kind)
          }

        case Arithmetic(op, kind) =>
          op match {
            case ADD => mcode.Emit(OpCodes.Add)
            case SUB => mcode.Emit(OpCodes.Sub)
            case MUL => mcode.Emit(OpCodes.Mul)
            case DIV => mcode.Emit(OpCodes.Div)
            case REM => mcode.Emit(OpCodes.Rem)
            case NOT => mcode.Emit(OpCodes.Not) //bitwise complement (one's complement)
            case _ => abort("Unknown arithmetic primitive " + primitive )
          }

        case Logical(op, kind) => op match {
          case AND => mcode.Emit(OpCodes.And)
          case OR => mcode.Emit(OpCodes.Or)
          case XOR => mcode.Emit(OpCodes.Xor)
        }

        case Shift(op, kind) => op match {
          case LSL => mcode.Emit(OpCodes.Shl)
          case ASR => mcode.Emit(OpCodes.Shr)
          case LSR => mcode.Emit(OpCodes.Shr_Un)
        }

        case Conversion(src, dst) =>
          debuglog("Converting from: " + src + " to: " + dst)

          dst match {
            case BYTE =>   mcode.Emit(OpCodes.Conv_I1) // I1 for System.SByte, i.e. a scala.Byte
            case SHORT =>  mcode.Emit(OpCodes.Conv_I2)
            case CHAR =>   mcode.Emit(OpCodes.Conv_U2)
            case INT =>    mcode.Emit(OpCodes.Conv_I4)
            case LONG =>   mcode.Emit(OpCodes.Conv_I8)
            case FLOAT =>  mcode.Emit(OpCodes.Conv_R4)
            case DOUBLE => mcode.Emit(OpCodes.Conv_R8)
            case _ =>
              Console.println("Illegal conversion at: " + clasz +
                              " at: " + pos.source + ":" + pos.line)
          }

        case ArrayLength(_) =>
          mcode.Emit(OpCodes.Ldlen)

        case StartConcat =>
          mcode.Emit(OpCodes.Newobj, MSTRING_BUILDER_CONSTR)


        case StringConcat(el) =>
          val elemType : MsilType = el match {
            case REFERENCE(_) | ARRAY(_) => MOBJECT
            case _ => msilType(el)
          }

          val argTypes:Array[MsilType] = Array(elemType)
          val stringBuilderAppend = MSTRING_BUILDER.GetMethod("Append", argTypes )
          mcode.Emit(OpCodes.Callvirt,  stringBuilderAppend)

        case EndConcat =>
          mcode.Emit(OpCodes.Callvirt, MSTRING_BUILDER_TOSTRING)

        case _ =>
          abort("Unimplemented primitive " + primitive)
      }
    } // end genPrimitive


    ////////////////////// loading ///////////////////////

    def loadI4(value: Int, code: ILGenerator): Unit = value match {
      case -1 => code.Emit(OpCodes.Ldc_I4_M1)
      case 0  => code.Emit(OpCodes.Ldc_I4_0)
      case 1  => code.Emit(OpCodes.Ldc_I4_1)
      case 2  => code.Emit(OpCodes.Ldc_I4_2)
      case 3  => code.Emit(OpCodes.Ldc_I4_3)
      case 4  => code.Emit(OpCodes.Ldc_I4_4)
      case 5  => code.Emit(OpCodes.Ldc_I4_5)
      case 6  => code.Emit(OpCodes.Ldc_I4_6)
      case 7  => code.Emit(OpCodes.Ldc_I4_7)
      case 8  => code.Emit(OpCodes.Ldc_I4_8)
      case _  =>
        if (value >= -128 && value <= 127)
          code.Emit(OpCodes.Ldc_I4_S, value)
        else
          code.Emit(OpCodes.Ldc_I4, value)
    }

    def loadArg(code: ILGenerator, loadAddr: Boolean)(i: Int) =
      if (loadAddr) {
        if (i >= -128 && i <= 127)
          code.Emit(OpCodes.Ldarga_S, i)
        else
          code.Emit(OpCodes.Ldarga, i)
      } else {
        i match {
          case 0 => code.Emit(OpCodes.Ldarg_0)
          case 1 => code.Emit(OpCodes.Ldarg_1)
          case 2 => code.Emit(OpCodes.Ldarg_2)
          case 3 => code.Emit(OpCodes.Ldarg_3)
          case _      =>
            if (i >= -128 && i <= 127)
              code.Emit(OpCodes.Ldarg_S, i)
            else
              code.Emit(OpCodes.Ldarg, i)
        }
      }

    def loadLocal(i: Int, local: Local, code: ILGenerator, loadAddr: Boolean) =
      if (loadAddr) {
        if (i >= -128 && i <= 127)
          code.Emit(OpCodes.Ldloca_S, localBuilders(local))
        else
          code.Emit(OpCodes.Ldloca, localBuilders(local))
      } else {
        i match {
          case 0 => code.Emit(OpCodes.Ldloc_0)
          case 1 => code.Emit(OpCodes.Ldloc_1)
          case 2 => code.Emit(OpCodes.Ldloc_2)
          case 3 => code.Emit(OpCodes.Ldloc_3)
          case _      =>
            if (i >= -128 && i <= 127)
              code.Emit(OpCodes.Ldloc_S, localBuilders(local))
            else
              code.Emit(OpCodes.Ldloc, localBuilders(local))
        }
      }

    ////////////////////// branches ///////////////////////

    /** Returns a Triple (Boolean, Boolean, Option[Label])
     *   - whether the jump leaves some exception block (try / catch / finally)
     *   - whether it leaves a finally handler (finally block, but not it's try / catch)
     *   - a label where to jump for leaving the finally handler
     *     . None to leave directly using `endfinally`
     *     . Some(label) to emit `leave label` (for try / catch inside a finally handler)
     */
    def leavesHandler(from: BasicBlock, to: BasicBlock): (Boolean, Boolean, Option[Label]) =
      if (currentHandlers.isEmpty) (false, false, None)
      else {
        val h = currentHandlers.head
        val leaveHead = { h.covers(from) != h.covers(to) ||
                          h.blocks.contains(from) != h.blocks.contains(to) }
        if (leaveHead) {
          // we leave the innermost exception block.
          // find out if we also leave som e `finally` handler
          currentHandlers.find(e => {
            e.cls == NoSymbol && e.blocks.contains(from) != e.blocks.contains(to)
          }) match {
            case Some(finallyHandler) =>
              if (h == finallyHandler) {
                // the finally handler is the innermost, so we can emit `endfinally` directly
                (true, true, None)
              } else {
                // we need to `Leave` to the `endfinally` of the next outer finally handler
                val l = endFinallyLabels.getOrElseUpdate(finallyHandler, mcode.DefineLabel())
                (true, true, Some(l))
              }
            case None =>
              (true, false, None)
          }
        } else (false, false, None)
      }

    def emitCondBr(block: BasicBlock, cond: TestOp, success: BasicBlock, failure: BasicBlock,
                   next: BasicBlock, emitBrFun: (TestOp, Label) => Unit) {
      val (sLeaveHandler, sLeaveFinally, slfTarget) = leavesHandler(block, success)
      val (fLeaveHandler, fLeaveFinally, flfTarget) = leavesHandler(block, failure)

      if (sLeaveHandler || fLeaveHandler) {
        val sLabelOpt = if (sLeaveHandler) {
          val leaveSLabel = mcode.DefineLabel()
          emitBrFun(cond, leaveSLabel)
          Some(leaveSLabel)
        } else {
          emitBrFun(cond, labels(success))
          None
        }

        if (fLeaveHandler) {
          if (fLeaveFinally) {
            if (flfTarget.isDefined) mcode.Emit(OpCodes.Leave, flfTarget.get)
            else mcode.Emit(OpCodes.Endfinally)
          } else
            mcode.Emit(OpCodes.Leave, labels(failure))
        } else
          mcode.Emit(OpCodes.Br, labels(failure))

        sLabelOpt.map(l => {
          mcode.MarkLabel(l)
          if (sLeaveFinally) {
            if (slfTarget.isDefined) mcode.Emit(OpCodes.Leave, slfTarget.get)
            else mcode.Emit(OpCodes.Endfinally)
          } else
            mcode.Emit(OpCodes.Leave, labels(success))
        })
      } else {
        if (next == success) {
          emitBrFun(cond.negate, labels(failure))
        } else {
          emitBrFun(cond, labels(success))
          if (next != failure) {
            mcode.Emit(OpCodes.Br, labels(failure))
          }
        }
      }
    }

    def emitBr(condition: TestOp, dest: Label, isFloat: Boolean) {
      condition match {
        case EQ => mcode.Emit(OpCodes.Beq, dest)
        case NE => mcode.Emit(OpCodes.Bne_Un, dest)
        case LT => mcode.Emit(if (isFloat) OpCodes.Blt_Un else OpCodes.Blt, dest)
        case GE => mcode.Emit(if (isFloat) OpCodes.Bge_Un else OpCodes.Bge, dest)
        case LE => mcode.Emit(if (isFloat) OpCodes.Ble_Un else OpCodes.Ble, dest)
        case GT => mcode.Emit(if (isFloat) OpCodes.Bgt_Un else OpCodes.Bgt, dest)
      }
    }

    def emitBrBool(cond: TestOp, dest: Label) {
      (cond: @unchecked) match {
        // EQ -> Brfalse, NE -> Brtrue; this is because we come from
        // a CZJUMP. If the value on the stack is 0 (e.g. a boolean
        // method returned false), and we are in the case EQ, then
        // we need to emit Brfalse (EQ Zero means false). vice versa
        case EQ => mcode.Emit(OpCodes.Brfalse, dest)
        case NE => mcode.Emit(OpCodes.Brtrue, dest)
      }
    }

    ////////////////////// local vars ///////////////////////

    /**
     * Compute the indexes of each local variable of the given
     * method.
     */
    def computeLocalVarsIndex(m: IMethod) {
      var idx = if (m.symbol.isStaticMember) 0 else 1

      val params = m.params
      for (l <- params) {
        debuglog("Index value for parameter " + l + ": " + idx)
        l.index = idx
        idx += 1 // sizeOf(l.kind)
      }

      val locvars = m.locals filterNot (params contains)
      idx = 0

      for (l <- locvars) {
        debuglog("Index value for local variable " + l + ": " + idx)
        l.index = idx
        idx += 1 // sizeOf(l.kind)
      }

    }

    ////////////////////// Utilities ////////////////////////

    /** Return the a name of this symbol that can be used on the .NET
     * platform. It removes spaces from names.
     *
     * Special handling: scala.All and scala.AllRef are 'erased' to
     * scala.All$ and scala.AllRef$. This is needed because they are
     * not real classes, and they mean 'abrupt termination upon evaluation
     * of that expression' or 'null' respectively. This handling is
     * done already in GenICode, but here we need to remove references
     * from method signatures to these types, because such classes can
     * not exist in the classpath: the type checker will be very confused.
     */
    def msilName(sym: Symbol): String = {
      val suffix = sym.moduleSuffix
      // Flags.JAVA: "symbol was not defined by a scala-class" (java, or .net-class)

      if (sym == definitions.NothingClass)
        return "scala.runtime.Nothing$"
      else if (sym == definitions.NullClass)
        return "scala.runtime.Null$"

      (if (sym.isClass || (sym.isModule && !sym.isMethod)) {
        if (sym.isNestedClass) sym.simpleName
        else sym.fullName
       } else
         sym.simpleName.toString.trim()) + suffix
    }


    ////////////////////// flags ///////////////////////

    def msilTypeFlags(sym: Symbol): Int = {
      var mf: Int = TypeAttributes.AutoLayout | TypeAttributes.AnsiClass

      if(sym.isNestedClass) {
        mf = mf | (if (sym hasFlag Flags.PRIVATE) TypeAttributes.NestedPrivate else TypeAttributes.NestedPublic)
      } else {
        mf = mf | (if (sym hasFlag Flags.PRIVATE) TypeAttributes.NotPublic else TypeAttributes.Public)
      }
      mf = mf | (if (sym hasFlag Flags.ABSTRACT) TypeAttributes.Abstract else 0)
      mf = mf | (if (sym.isTrait && !sym.isImplClass) TypeAttributes.Interface else TypeAttributes.Class)
      mf = mf | (if (sym isFinal) TypeAttributes.Sealed else 0)

      sym.annotations foreach { a => a match {
        case AnnotationInfo(SerializableAttr, _, _) =>
          // TODO: add the Serializable TypeAttribute also if the annotation
          // System.SerializableAttribute is present (.net annotation, not scala)
          //  Best way to do it: compare with
          //  definitions.getClass("System.SerializableAttribute").tpe
          //  when frontend available
          mf = mf | TypeAttributes.Serializable
        case _ => ()
      }}

      mf
      // static: not possible (or?)
    }

    def msilMethodFlags(sym: Symbol): Short = {
      var mf: Int = MethodAttributes.HideBySig |
        (if (sym hasFlag Flags.PRIVATE) MethodAttributes.Private
         else MethodAttributes.Public)

      if (!sym.isClassConstructor) {
        if (sym.isStaticMember)
          mf = mf | FieldAttributes.Static // coincidentally, same value as for MethodAttributes.Static ...
        else {
          mf = mf | MethodAttributes.Virtual
          if (sym.isFinal && !getType(sym.owner).IsInterface)
            mf = mf | MethodAttributes.Final
          if (sym.isDeferred || getType(sym.owner).IsInterface)
            mf = mf | MethodAttributes.Abstract
        }
      }

      if (sym.isStaticMember) {
        mf = mf | MethodAttributes.Static
      }

      // constructors of module classes should be private
      if (sym.isPrimaryConstructor && isTopLevelModule(sym.owner)) {
        mf |= MethodAttributes.Private
        mf &= ~(MethodAttributes.Public)
      }

      mf.toShort
    }

    def msilFieldFlags(sym: Symbol): Short = {
      var mf: Int =
        if (sym hasFlag Flags.PRIVATE) FieldAttributes.Private
        else if (sym hasFlag Flags.PROTECTED) FieldAttributes.FamORAssem
        else FieldAttributes.Public

      if (sym hasFlag Flags.FINAL)
        mf = mf | FieldAttributes.InitOnly

      if (sym.isStaticMember)
        mf = mf | FieldAttributes.Static

      // TRANSIENT: "not serialized", VOLATILE: doesn't exist on .net
      // TODO: add this annotation also if the class has the custom attribute
      // System.NotSerializedAttribute
      sym.annotations.foreach( a => a match {
        case AnnotationInfo(TransientAtt, _, _) =>
          mf = mf | FieldAttributes.NotSerialized
        case _ => ()
      })

      mf.toShort
    }

    ////////////////////// builders, types ///////////////////////

    var entryPoint: Symbol = _

    val notInitializedModules = mutable.HashSet[Symbol]()

    // TODO: create fields also in def createType, and not in genClass,
    // add a getField method (it only works as it is because fields never
    // accessed from outside a class)

    val localBuilders = mutable.HashMap[Local, LocalBuilder]()

    private[GenMSIL] def findEntryPoint(cls: IClass) {

      def isEntryPoint(sym: Symbol):Boolean = {
        if (isStaticModule(sym.owner) && msilName(sym) == "main")
          if (sym.tpe.paramTypes.length == 1) {
            toTypeKind(sym.tpe.paramTypes(0)) match {
              case ARRAY(elem) =>
                if (elem.toType.typeSymbol == definitions.StringClass) {
                  return true
                }
              case _ => ()
            }
          }
        false
      }

      if((entryPoint == null) && opt.showClass.isDefined) {  // TODO introduce dedicated setting instead
        val entryclass = opt.showClass.get.toString
        val cfn = cls.symbol.fullName
        if(cfn == entryclass) {
          for (m <- cls.methods; if isEntryPoint(m.symbol)) { entryPoint = m.symbol }
          if(entryPoint == null) { warning("Couldn't find main method in class " + cfn) }
        }
      }

      if (firstSourceName == "")
        if (cls.symbol.sourceFile != null) // is null for nested classes
          firstSourceName = cls.symbol.sourceFile.name
    }

    // #####################################################################
    // get and create types

    private def msilType(t: TypeKind): MsilType = (t: @unchecked) match {
      case UNIT           => MVOID
      case BOOL           => MBOOL
      case BYTE           => MBYTE
      case SHORT          => MSHORT
      case CHAR           => MCHAR
      case INT            => MINT
      case LONG           => MLONG
      case FLOAT          => MFLOAT
      case DOUBLE         => MDOUBLE
      case REFERENCE(cls) => getType(cls)
      case ARRAY(elem)    =>
        msilType(elem) match {
          // For type builders, cannot call "clrTypes.mkArrayType" because this looks up
          // the type "tp" in the assembly (not in the HashMap "types" of the backend).
          // This can fail for nested types because the builders are not complete yet.
          case tb: TypeBuilder => tb.MakeArrayType()
          case tp: MsilType => clrTypes.mkArrayType(tp)
        }
    }

    private def msilType(tpe: Type): MsilType = msilType(toTypeKind(tpe))

    private def msilParamTypes(sym: Symbol): Array[MsilType] = {
      sym.tpe.paramTypes.map(msilType).toArray
    }

    def getType(sym: Symbol) = getTypeOpt(sym).getOrElse(abort(showsym(sym)))

    /**
     * Get an MSIL type from a symbol. First look in the clrTypes.types map, then
     * lookup the name using clrTypes.getType
     */
    def getTypeOpt(sym: Symbol): Option[MsilType] = {
      val tmp = types.get(sym)
      tmp match {
        case typ @ Some(_) => typ
        case None =>
          def typeString(sym: Symbol): String = {
            val s = if (sym.isNestedClass) typeString(sym.owner) +"+"+ sym.simpleName
                    else sym.fullName
            if (sym.isModuleClass && !sym.isTrait) s + "$" else s
          }
          val name = typeString(sym)
          val typ = clrTypes.getType(name)
          if (typ == null)
            None
          else {
            types(sym) = typ
            Some(typ)
          }
      }
    }

    def mapType(sym: Symbol, mType: MsilType) {
      assert(mType != null, showsym(sym))
      types(sym) = mType
    }

    def createTypeBuilder(iclass: IClass) {
      /**
       * First look in the clrTypes.types map, if that fails check if it's a class being compiled, otherwise
       * lookup by name (clrTypes.getType calls the static method msil.Type.GetType(fullname)).
       */
      def msilTypeFromSym(sym: Symbol): MsilType = {
        types.get(sym).getOrElse {
          classes.get(sym) match {
            case Some(iclass) =>
	              msilTypeBuilderFromSym(sym)
            case None =>
              getType(sym)
          }
        }
      }

      def msilTypeBuilderFromSym(sym: Symbol): TypeBuilder = {
        if(!(types.contains(sym) && types(sym).isInstanceOf[TypeBuilder])){
          val iclass = classes(sym)
          assert(iclass != null)
          createTypeBuilder(iclass)
        }
        types(sym).asInstanceOf[TypeBuilder]
      }

      val sym = iclass.symbol
      if (types.contains(sym) && types(sym).isInstanceOf[TypeBuilder])
        return

      def isInterface(s: Symbol) = s.isTrait && !s.isImplClass
      val parents: List[Type] =
        if (sym.info.parents.isEmpty) List(definitions.ObjectClass.tpe)
        else sym.info.parents.distinct

      val superType : MsilType = if (isInterface(sym)) null else msilTypeFromSym(parents.head.typeSymbol)
      debuglog("super type: " + parents(0).typeSymbol + ", msil type: " + superType)

      val interfaces: Array[MsilType] =
	parents.tail.map(p => msilTypeFromSym(p.typeSymbol)).toArray
      if (parents.length > 1) {
        if (settings.debug.value) {
          log("interfaces:")
          for (i <- 0.until(interfaces.length)) {
            log("  type: " + parents(i + 1).typeSymbol + ", msil type: " + interfaces(i))
          }
        }
      }

      val tBuilder = if (sym.isNestedClass) {
        val ownerT = msilTypeBuilderFromSym(sym.owner).asInstanceOf[TypeBuilder]
        ownerT.DefineNestedType(msilName(sym), msilTypeFlags(sym), superType, interfaces)
      } else {
        mmodule.DefineType(msilName(sym), msilTypeFlags(sym), superType, interfaces)
      }
      mapType(sym, tBuilder)
    } // createTypeBuilder

    def createClassMembers(iclass: IClass) {
      try {
        createClassMembers0(iclass)
      }
      catch {
        case e: Throwable =>
          java.lang.System.err.println(showsym(iclass.symbol))
          java.lang.System.err.println("with methods = " + iclass.methods)
          throw e
      }
    }

    def createClassMembers0(iclass: IClass) {

      val mtype = getType(iclass.symbol).asInstanceOf[TypeBuilder]

      for (ifield <- iclass.fields) {
        val sym = ifield.symbol
        debuglog("Adding field: " + sym.fullName)

        var attributes = msilFieldFlags(sym)
        val fieldTypeWithCustomMods =
          new PECustomMod(msilType(sym.tpe),
                          customModifiers(sym.annotations))
        val fBuilder = mtype.DefineField(msilName(sym),
                                         fieldTypeWithCustomMods,
                                         attributes)
        fields(sym) = fBuilder
        addAttributes(fBuilder, sym.annotations)
      } // all iclass.fields iterated over

      if (isStaticModule(iclass.symbol)) {
        val sc = iclass.lookupStaticCtor
        if (sc.isDefined) {
          val m = sc.get
          val oldLastBlock = m.lastBlock
          val lastBlock = m.newBlock()
          oldLastBlock.replaceInstruction(oldLastBlock.length - 1, JUMP(lastBlock))
          // call object's private ctor from static ctor
          lastBlock.emit(CIL_NEWOBJ(iclass.symbol.primaryConstructor))
          lastBlock.emit(DROP(toTypeKind(iclass.symbol.tpe)))
          lastBlock emit RETURN(UNIT)
          lastBlock.close
        }
      }

      if (iclass.symbol != definitions.ArrayClass) {
      for (m: IMethod <- iclass.methods) {
        val sym = m.symbol
        debuglog("Creating MethodBuilder for " + Flags.flagsToString(sym.flags) + " " +
              sym.owner.fullName + "::" + sym.name)

        val ownerType = getType(sym.enclClass).asInstanceOf[TypeBuilder]
        assert(mtype == ownerType, "mtype = " + mtype + "; ownerType = " + ownerType)
        var paramTypes = msilParamTypes(sym)
        val attr = msilMethodFlags(sym)

        if (m.symbol.isClassConstructor) {
          val constr =
            ownerType.DefineConstructor(attr, CallingConventions.Standard, paramTypes)
          for (i <- 0.until(paramTypes.length)) {
            constr.DefineParameter(i, ParameterAttributes.None, msilName(m.params(i).sym))
          }
          mapConstructor(sym, constr)
          addAttributes(constr, sym.annotations)
        } else {
          var resType = msilType(m.returnType)
          val method =
            ownerType.DefineMethod(msilName(sym), attr, resType, paramTypes)
          for (i <- 0.until(paramTypes.length)) {
            method.DefineParameter(i, ParameterAttributes.None, msilName(m.params(i).sym))
          }
          if (!methods.contains(sym))
            mapMethod(sym, method)
          addAttributes(method, sym.annotations)
          debuglog("\t created MethodBuilder " + method)
        }
      }
      } // method builders created for non-array iclass

      if (isStaticModule(iclass.symbol)) {
        addModuleInstanceField(iclass.symbol)
        notInitializedModules += iclass.symbol
        if (iclass.lookupStaticCtor.isEmpty) {
          addStaticInit(iclass.symbol)
        }
      }

    } // createClassMembers0

    private def isTopLevelModule(sym: Symbol): Boolean =
      beforeRefchecks {
        sym.isModuleClass && !sym.isImplClass && !sym.isNestedClass
      }

    // if the module is lifted it does not need to be initialized in
    // its static constructor, and the MODULE$ field is not required.
    // the outer class will care about it.
    private def isStaticModule(sym: Symbol): Boolean = {
      // .net inner classes: removed '!sym.hasFlag(Flags.LIFTED)', added
      // 'sym.isStatic'. -> no longer compatible without skipping flatten!
      sym.isModuleClass && sym.isStatic && !sym.isImplClass
    }

    private def isCloneable(sym: Symbol): Boolean = {
      !sym.annotations.forall( a => a match {
        case AnnotationInfo(CloneableAttr, _, _) => false
        case _ => true
      })
    }

    private def addModuleInstanceField(sym: Symbol) {
      debuglog("Adding Module-Instance Field for " + showsym(sym))
      val tBuilder = getType(sym).asInstanceOf[TypeBuilder]
      val fb = tBuilder.DefineField(MODULE_INSTANCE_NAME,
                           tBuilder,
                           (FieldAttributes.Public |
                            //FieldAttributes.InitOnly |
                            FieldAttributes.Static).toShort)
      fields(sym) = fb
    }


    // the symbol may be a object-symbol (module-symbol), or a module-class-symbol
    private def getModuleInstanceField(sym: Symbol): FieldInfo = {
      assert(sym.isModule || sym.isModuleClass, "Expected module: " + showsym(sym))

      // when called by LOAD_MODULE, the corresponding type maybe doesn't
      // exist yet -> make a getType
      val moduleClassSym = if (sym.isModule) sym.moduleClass else sym

      // TODO: get module field for modules not defined in the
      // source currently compiling (e.g. Console)

      fields get moduleClassSym match {
        case Some(sym) => sym
        case None =>
          //val mclass = types(moduleClassSym)
          val nameInMetadata = nestingAwareFullClassname(moduleClassSym)
          val mClass = clrTypes.getType(nameInMetadata)
          val mfield = mClass.GetField("MODULE$")
          assert(mfield ne null, "module not found " + showsym(moduleClassSym))
          fields(moduleClassSym) = mfield
          mfield
      }

      //fields(moduleClassSym)
    }

    def nestingAwareFullClassname(csym: Symbol) : String = {
      val suffix = csym.moduleSuffix
      val res = if (csym.isNestedClass)
        nestingAwareFullClassname(csym.owner) + "+" + csym.encodedName
      else
        csym.fullName
      res + suffix
    }

    /** Adds a static initializer which creates an instance of the module
     *  class (calls the primary constructor). A special primary constructor
     *  will be generated (notInitializedModules) which stores the new instance
     *  in the MODULE$ field right after the super call.
     */
    private def addStaticInit(sym: Symbol) {
      val tBuilder = getType(sym).asInstanceOf[TypeBuilder]

      val staticInit = tBuilder.DefineConstructor(
        (MethodAttributes.Static | MethodAttributes.Public).toShort,
        CallingConventions.Standard,
        MsilType.EmptyTypes)

      val sicode = staticInit.GetILGenerator()

      val instanceConstructor = constructors(sym.primaryConstructor)

      // there are no constructor parameters. assuming the constructor takes no parameter
      // is fine: we call (in the static constructor) the constructor of the module class,
      // which takes no arguments - an object definition cannot take constructor arguments.
      sicode.Emit(OpCodes.Newobj, instanceConstructor)
      // the stsfld is done in the instance constructor, just after the super call.
      sicode.Emit(OpCodes.Pop)

      sicode.Emit(OpCodes.Ret)
    }

    private def generateMirrorClass(sym: Symbol) {
      val tBuilder = getType(sym)
      assert(sym.isModuleClass, "Can't generate Mirror-Class for the Non-Module class " + sym)
      debuglog("Dumping mirror class for object: " + sym)
      val moduleName = msilName(sym)
      val mirrorName = moduleName.substring(0, moduleName.length() - 1)
      val mirrorTypeBuilder = mmodule.DefineType(mirrorName,
                                                 TypeAttributes.Class |
                                                 TypeAttributes.Public |
                                                 TypeAttributes.Sealed,
                                                 MOBJECT,
                                                 MsilType.EmptyTypes)

      val iclass = classes(sym)

      for (m <- sym.tpe.nonPrivateMembers
           if m.owner != definitions.ObjectClass && !m.isProtected &&
           m.isMethod && !m.isClassConstructor && !m.isStaticMember && !m.isCase &&
           !m.isDeferred)
        {
          debuglog("   Mirroring method: " + m)
          val paramTypes = msilParamTypes(m)
          val paramNames: Array[String] = new Array[String](paramTypes.length)
          for (i <- 0 until paramTypes.length)
            paramNames(i) = "x_" + i

          // CHECK: verify if getMethodName is better than msilName
          val mirrorMethod = mirrorTypeBuilder.DefineMethod(msilName(m),
                                                            (MethodAttributes.Public |
                                                            MethodAttributes.Static).toShort,
                                                            msilType(m.tpe.resultType),
                                                            paramTypes)

          var i = 0
          while (i < paramTypes.length) {
            mirrorMethod.DefineParameter(i, ParameterAttributes.None, paramNames(i))
            i += 1
          }

          val mirrorCode = mirrorMethod.GetILGenerator()
          mirrorCode.Emit(OpCodes.Ldsfld, getModuleInstanceField(sym))
          val mInfo = getMethod(m)
          for (paramidx <- 0.until(paramTypes.length)) {
            val mInfoParams = mInfo.GetParameters
            val loadAddr = mInfoParams(paramidx).ParameterType.IsByRef
            loadArg(mirrorCode, loadAddr)(paramidx)
          }

          mirrorCode.Emit(OpCodes.Callvirt, getMethod(m))
          mirrorCode.Emit(OpCodes.Ret)
        }

      addSymtabAttribute(sym.sourceModule, mirrorTypeBuilder)

      mirrorTypeBuilder.CreateType()
      mirrorTypeBuilder.setSourceFilepath(iclass.cunit.source.file.path)
    }


    // #####################################################################
    // delegate callers

    var delegateCallers: TypeBuilder = _
    var nbDelegateCallers: Int = 0

    private def initDelegateCallers() = {
      delegateCallers = mmodule.DefineType("$DelegateCallers", TypeAttributes.Public |
                                          TypeAttributes.Sealed)
    }

    private def createDelegateCaller(functionType: Type, delegateType: Type) = {
      if (delegateCallers == null)
        initDelegateCallers()
      // create a field an store the function-object
      val mFunctionType: MsilType = msilType(functionType)
      val anonfunField: FieldBuilder = delegateCallers.DefineField(
        "$anonfunField$$" + nbDelegateCallers, mFunctionType,
        (FieldAttributes.InitOnly | FieldAttributes.Public | FieldAttributes.Static).toShort)
      mcode.Emit(OpCodes.Stsfld, anonfunField)


      // create the static caller method and the delegate object
      val (params, returnType) = delegateType.member(nme.apply).tpe match {
        case MethodType(delParams, delReturn) => (delParams, delReturn)
        case _ => abort("not a delegate type: "  + delegateType)
      }
      val caller: MethodBuilder = delegateCallers.DefineMethod(
        "$delegateCaller$$" + nbDelegateCallers,
        (MethodAttributes.Final | MethodAttributes.Public | MethodAttributes.Static).toShort,
        msilType(returnType), (params map (_.tpe)).map(msilType).toArray)
      for (i <- 0 until params.length)
        caller.DefineParameter(i, ParameterAttributes.None, "arg" + i) // FIXME: use name of parameter symbol
      val delegCtor = msilType(delegateType).GetConstructor(Array(MOBJECT, INT_PTR))
      mcode.Emit(OpCodes.Ldnull)
      mcode.Emit(OpCodes.Ldftn, caller)
      mcode.Emit(OpCodes.Newobj, delegCtor)


      // create the static caller method body
      val functionApply: MethodInfo = getMethod(functionType.member(nme.apply))
      val dcode: ILGenerator = caller.GetILGenerator()
      dcode.Emit(OpCodes.Ldsfld, anonfunField)
      for (i <- 0 until params.length) {
        loadArg(dcode, false /* TODO confirm whether passing actual as-is to formal is correct wrt the ByRef attribute of the param */)(i)
        emitBox(dcode, toTypeKind(params(i).tpe))
      }
      dcode.Emit(OpCodes.Callvirt, functionApply)
      emitUnbox(dcode, toTypeKind(returnType))
      dcode.Emit(OpCodes.Ret)

      nbDelegateCallers = nbDelegateCallers + 1

    } //def createDelegateCaller

    def emitBox(code: ILGenerator, boxType: TypeKind) = (boxType: @unchecked) match {
      // doesn't make sense, unit as parameter..
      case UNIT   => code.Emit(OpCodes.Ldsfld, boxedUnit)
      case BOOL | BYTE | SHORT | CHAR | INT | LONG | FLOAT | DOUBLE =>
        code.Emit(OpCodes.Box, msilType(boxType))
      case REFERENCE(cls) if clrTypes.isValueType(cls) =>
        code.Emit(OpCodes.Box, (msilType(boxType)))
      case REFERENCE(_) | ARRAY(_) =>
        warning("Tried to BOX a non-valuetype.")
        ()
    }

    def emitUnbox(code: ILGenerator, boxType: TypeKind) = (boxType: @unchecked) match {
      case UNIT   => code.Emit(OpCodes.Pop)
      /* (1) it's essential to keep the code emitted here (as of now plain calls to System.Convert.ToBlaBla methods)
             behaviorally.equiv.wrt. BoxesRunTime.unboxToBlaBla methods
             (case null: that's easy, case boxed: track changes to unboxBlaBla)
         (2) See also: asInstanceOf to cast from Any to number,
             tracked in http://lampsvn.epfl.ch/trac/scala/ticket/4437  */
      case BOOL   => code.Emit(OpCodes.Call, toBool)
      case BYTE   => code.Emit(OpCodes.Call, toSByte)
      case SHORT  => code.Emit(OpCodes.Call, toShort)
      case CHAR   => code.Emit(OpCodes.Call, toChar)
      case INT    => code.Emit(OpCodes.Call, toInt)
      case LONG   => code.Emit(OpCodes.Call, toLong)
      case FLOAT  => code.Emit(OpCodes.Call, toFloat)
      case DOUBLE => code.Emit(OpCodes.Call, toDouble)
      case REFERENCE(cls) if clrTypes.isValueType(cls) =>
        code.Emit(OpCodes.Unbox, msilType(boxType))
        code.Emit(OpCodes.Ldobj, msilType(boxType))
      case REFERENCE(_) | ARRAY(_) =>
        warning("Tried to UNBOX a non-valuetype.")
        ()
    }

    // #####################################################################
    // get and create methods / constructors

    def getConstructor(sym: Symbol): ConstructorInfo = constructors.get(sym) match {
      case Some(constr) => constr
      case None =>
        val mClass = getType(sym.owner)
        val constr = mClass.GetConstructor(msilParamTypes(sym))
        if (constr eq null) {
          java.lang.System.out.println("Cannot find constructor " + sym.owner + "::" + sym.name)
          java.lang.System.out.println("scope = " + sym.owner.tpe.decls)
          abort(sym.fullName)
        }
        else {
          mapConstructor(sym, constr)
          constr
        }
    }

    def mapConstructor(sym: Symbol, cInfo: ConstructorInfo) = {
      constructors(sym) = cInfo
    }

    private def getMethod(sym: Symbol): MethodInfo = {

        methods.get(sym) match {
        case Some(method) => method
        case None =>
          val mClass = getType(sym.owner)
          try {
            val method = mClass.GetMethod(msilName(sym), msilParamTypes(sym),
                                          msilType(sym.tpe.resultType))
            if (method eq null) {
              java.lang.System.out.println("Cannot find method " + sym.owner + "::" + msilName(sym))
              java.lang.System.out.println("scope = " + sym.owner.tpe.decls)
              abort(sym.fullName)
            }
            else {
              mapMethod(sym, method)
              method
            }
          }
          catch {
            case e: Exception =>
              Console.println("While looking up " + mClass + "::" + sym.nameString)
            Console.println("\t" + showsym(sym))
            throw e
          }
      }
    }

    /*
     * add a mapping between sym and mInfo
     */
    private def mapMethod(sym: Symbol, mInfo: MethodInfo) {
      assert (mInfo != null, mInfo)
      methods(sym) = mInfo
    }

    /*
     * add mapping between sym and method with newName, paramTypes of newClass
     */
    private def mapMethod(sym: Symbol, newClass: MsilType, newName: String, paramTypes: Array[MsilType]) {
      val methodInfo = newClass.GetMethod(newName, paramTypes)
      assert(methodInfo != null, "Can't find mapping for " + sym + " -> " +
             newName + "(" + paramTypes + ")")
      mapMethod(sym, methodInfo)
      if (methodInfo.IsStatic)
        dynToStatMapped += sym
    }

    /*
     * add mapping between method with name and paramTypes of clazz to
     * method with newName and newParamTypes of newClass (used for instance
     * for "wait")
     */
    private def mapMethod(
      clazz: Symbol, name: Name, paramTypes: Array[Type],
      newClass: MsilType, newName: String, newParamTypes: Array[MsilType]) {
        val methodSym = lookupMethod(clazz, name, paramTypes)
        assert(methodSym != null, "cannot find method " + name + "(" +
               paramTypes + ")" + " in class " + clazz)
        mapMethod(methodSym, newClass, newName, newParamTypes)
      }

    /*
     * add mapping for member with name and paramTypes to member
     * newName of newClass (same parameters)
     */
    private def mapMethod(
      clazz: Symbol, name: Name, paramTypes: Array[Type],
      newClass: MsilType, newName: String) {
        mapMethod(clazz, name, paramTypes, newClass, newName, paramTypes map msilType)
      }

    /*
     * add mapping for all methods with name of clazz to the corresponding
     * method (same parameters) with newName of newClass
     */
    private def mapMethod(
      clazz: Symbol, name: Name,
      newClass: MsilType, newName: String) {
        val memberSym: Symbol = clazz.tpe.member(name)
        memberSym.tpe match {
          // alternatives: List[Symbol]
          case OverloadedType(_, alternatives) =>
            alternatives.foreach(s => mapMethod(s, newClass, newName, msilParamTypes(s)))

          // paramTypes: List[Type], resType: Type
          case MethodType(params, resType) =>
            mapMethod(memberSym, newClass, newName, msilParamTypes(memberSym))

          case _ =>
            abort("member not found: " + clazz + ", " + name)
        }
      }


    /*
     * find the method in clazz with name and paramTypes
     */
    private def lookupMethod(clazz: Symbol, name: Name, paramTypes: Array[Type]): Symbol = {
      val memberSym = clazz.tpe.member(name)
      memberSym.tpe match {
        case OverloadedType(_, alternatives) =>
          alternatives.find(s => {
            var i: Int = 0
            var typesOK: Boolean = true
            if (paramTypes.length == s.tpe.paramTypes.length) {
              while(i < paramTypes.length) {
                if (paramTypes(i) != s.tpe.paramTypes(i))
                  typesOK = false
                i += 1
              }
            } else {
              typesOK = false
            }
            typesOK
          }) match {
            case Some(sym) => sym
            case None => abort("member of " + clazz + ", " + name + "(" +
                               paramTypes + ") not found")
          }

        case MethodType(_, _) => memberSym

        case _ => abort("member not found: " + name + " of " + clazz)
      }
    }

    private def showsym(sym: Symbol): String = (sym.toString +
      "\n  symbol = " + Flags.flagsToString(sym.flags) + " " + sym +
      "\n  owner  = " + Flags.flagsToString(sym.owner.flags) + " " + sym.owner
    )

  } // class BytecodeGenerator

} // class GenMSIL
