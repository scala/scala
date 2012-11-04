/*
 * System.Reflection.Emit-like API for writing .NET assemblies in MSIL
 */


package ch.epfl.lamp.compiler.msil.emit

import java.io.File
import java.io.FileWriter
import java.io.BufferedWriter
import java.io.PrintWriter
import java.io.IOException
import java.util.Comparator

import ch.epfl.lamp.compiler.msil._
import ch.epfl.lamp.compiler.msil.util.Table

/**
 * The MSIL printer Visitor. It prints a complete
 * assembly in a single or multiple files. Then this file can be compiled by ilasm.
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
abstract class ILPrinterVisitor extends Visitor {

    import ILPrinterVisitor._
    import OpCode._

    //##########################################################################

    protected final val assemblyNameComparator =
        new scala.math.Ordering[Assembly]() {
            override def compare(o1: Assembly, o2: Assembly): Int = {
                val a1 = o1.asInstanceOf[Assembly]
                val a2 = o2.asInstanceOf[Assembly]
                return a1.GetName().Name.compareTo(a2.GetName().Name)
            }
        }

    // the output file writer
    protected var out: PrintWriter = null

    // the left margin
    private var lmargin = 0

    // indicate a newline
    private var newline = true

    // print types without or with members?
    protected var nomembers: Boolean = false

    // external assemblies
    protected var as: Array[Assembly] = null

    private def align() {
	if (newline)
	    padding = lmargin
	printPadding()
	newline = false
    }
    private def indent() {
	lmargin += TAB
    }
    private def undent() {
	lmargin -= TAB
	assert(lmargin >= 0)
    }

    private var padding = 0
    private def pad(n: Int) {
	assert(n >= 0, "negative padding: " + n)
        padding += n
    }
    private def printPadding() {
	if (padding <= 0)
	    return
	while (padding > SPACES_LEN) {
	    out.print(SPACES)
	    padding -= SPACES_LEN
	}
        out.print(SPACES.substring(0, padding))
        padding = 0
    }

    // methods to print code
    protected def print(s: String)  { align(); out.print(s)}
    protected def print(o: Object)  { align(); out.print(o) }
    protected def print(c: Char)    { align(); out.print(c) }
    protected def print(`val`: Int) { align(); out.print(`val`)}
    protected def print(`val`: Long){ align(); out.print(`val`)}
    protected def println()         { out.println(); newline = true; padding = 0 }
    protected def println(c: Char)  { print(c); println() }
    protected def println(i: Int)   { print(i); println() }
    protected def println(l: Long)  { print(l); println() }
    protected def println(s: String){ print(s); println() }
    protected def println(o: Object){ print(o); println() }
    protected def printName(name: String) {
        val ch = name.charAt(0)
	//if (Character.isLetter(ch) && Character.isLowerCase(ch)) {
	if ((ch != '.') && (ch != '!')) {
	    print('\''); print(name); print('\'')
	} else
	    print(name)
    }

	protected def printAssemblyBoilerplate() {
	// print all the external assemblies
    for (j <- 0 until as.length) {
	    printAssemblySignature(as(j), true)
	}
	// print assembly declaration
	printAssemblySignature(currAssembly, false)
	}

    // the entrypoint method
    protected var entryPoint: MethodInfo = null

    // current opcode argument
    protected var argument: Object = null

    /***/
    @throws(classOf[IOException])
    protected def print(vAble: Visitable) {
	if (vAble != null)
	    vAble.apply(this)
    }

    /**
     * Visit an AssemblyBuilder
     */
    @throws(classOf[IOException])
    def caseAssemblyBuilder(assemblyBuilder: AssemblyBuilder)

    protected var currentModule: Module = null
    /**
     * Visit a ModuleBuilder
     */
    @throws(classOf[IOException])
    def caseModuleBuilder(module: ModuleBuilder)

    protected var currentType: Type = null

  def printTypeParams(sortedTVars : Array[GenericParamAndConstraints]) {

    def constraintFlags(tVar : GenericParamAndConstraints) = {
      val varianceDirective = (if (tVar.isCovariant) "+ " else (if (tVar.isContravariant) "- " else ""))
      val typeKindDirective = (if (tVar.isReferenceType) "class " else (if (tVar.isValueType) "valuetype " else ""))
      val dfltConstrDirective = (if (tVar.hasDefaultConstructor) ".ctor " else "")
      varianceDirective + typeKindDirective + dfltConstrDirective
    }

    def tparamName(tVar : GenericParamAndConstraints) = {
     /* TODO Type-params in referenced assemblies may lack a name (those in a TypeBuilder or MethodBuilder shouldn't).
        Given that we need not list (in ilasm syntax) the original type-params' names when
         providing type arguments to it, the only type-param-names we'll serialize into a .msil file
         are those for type-params in a TypeBuilder or MethodBuilder. Still, more details on this
         appear in Sec. 4.5 "Faulty metadata in XMLReaderFactory" of
         http://lamp.epfl.ch/~magarcia/ScalaCompilerCornerReloaded/Libs4Lib.pdf

        To avoid name clashes when choosing a param name,
        first collect all existing tparam-names from a type (and its nested types).
        Not that those names are needed (ordinal positions can be used instead)
        but will look better when disassembling with ildasm. */
      assert(tVar.Name != null)
      tVar.Name
    }

    if(sortedTVars.length == 0) { return }
    print('<')
    val lastIdx = sortedTVars.length - 1
    for (it <- 0 until sortedTVars.length) {
      val tVar = sortedTVars(it)
      print(constraintFlags(tVar))
      if(tVar.Constraints.length > 0) {
        print('(')
        for (ic <- 0 until tVar.Constraints.length) {
          val cnstrt = tVar.Constraints(ic)
          printReference(cnstrt)
          if (ic < lastIdx) { print(", ") }
        }
        print(')')
      }
      print(" " + tparamName(tVar))
      if (it < lastIdx) { print(", ") }
    }
    print('>')
  }

    /**
     * Visit a TypeBuilder
     */
    @throws(classOf[IOException])
    def caseTypeBuilder(`type`: TypeBuilder) {
	currentType = `type`
	if (!`type`.Namespace.equals("") && `type`.DeclaringType == null) {
	    print(".namespace \'" ); print(`type`.Namespace); println("\'")
	    println("{"); indent()
	}
	print(".class ")
	// <classHead> ::=
	// <classAttr>* <id>
	// [extends <typeReference>]
	// [implements <typeReference> [, <typeReference>]*]
	print(TypeAttributes.toString(`type`.Attributes))
	print(" \'"); print(`type`.Name); print("\'")
    printTypeParams(`type`.getSortedTVars())
	if (`type`.BaseType() != null) {
	    println()
	    print("       extends    ")
	    printReference(`type`.BaseType())
	}
        val ifaces: Array[Type] = `type`.getInterfaces()
	if (ifaces.length > 0) {
	    println()
	    print("       implements ")
        for (i <- 0 until ifaces.length) {
		if (i > 0) {
		    println(",")
		    print("                  ")
		}
		printReference(ifaces(i))
	    }
	}
	println()
	println("{")
	indent()
	if (!nomembers && `type`.sourceFilename != null)
	    println(".line  " + `type`.sourceLine
		    + "  '" + `type`.sourceFilename + "'")
        if (!nomembers) {
            printAttributes(`type`)
        }
	// print nested classes
    val nested = `type`.nestedTypeBuilders.iterator
    while(nested.hasNext)
      print(nested.next().asInstanceOf[TypeBuilder])

	// print each field
    val fields = `type`.fieldBuilders.iterator
    while(fields.hasNext)
	  print(fields.next().asInstanceOf[FieldBuilder])

	// print each constructor
	val constrs = `type`.constructorBuilders.iterator
	while (constrs.hasNext)
	    print(constrs.next().asInstanceOf[ConstructorBuilder])

	// print each method
	val methods = `type`.methodBuilders.iterator
	while (methods.hasNext) {
	    val method = methods.next().asInstanceOf[MethodBuilder]
	    assert(method.DeclaringType == `type`)
	    print(method)
	}

	undent(); println("}")
	if (!`type`.Namespace.equals("") && `type`.DeclaringType == null) {
	    undent(); println("}")
	}
	currentType = null
    }

    /**
     * Visit a FieldBuilder
     */
    @throws(classOf[IOException])
    def caseFieldBuilder(field: FieldBuilder) {
        if (nomembers) return
	// [[int32]] <fieldAttr>* <type> <id> [= <fieldInit> | at <dataLabel>]
	print(".field ")
	print(FieldAttributes.toString(field.Attributes))
	print(" "); printSignature(field.FieldType, field.cmods)
	print(" \'"); print(field.Name); print("\'")
	if (field.IsLiteral()) {
	    print(" = ")
	    val value = field.getValue()
	    if (value == null) {
		print("nullref")
	    } else if (value.isInstanceOf[String]) {
		print(msilString(value.asInstanceOf[String]))
	    } else if (value.isInstanceOf[Boolean]) {
		print("bool (")
		print(if((value.asInstanceOf[Boolean]).booleanValue()) { "true" } else { "false" })
		print(")")
	    } else if (value.isInstanceOf[Byte]) {
		print("int8 (")
		print(value)
		print(")")
	    } else if (value.isInstanceOf[java.lang.Short]) {
		print("int16 (")
		print(value)
		print(")")
	    } else if (value.isInstanceOf[Character]) {
		print("char (")
		print((value.asInstanceOf[Character]).charValue())
		print(")")
	    } else if (value.isInstanceOf[Integer]) {
		print("int32 (")
		print((value.asInstanceOf[Integer]).intValue())
		print(")")
	    } else if (value.isInstanceOf[Long]) {
		print("int64 (")
		print((value.asInstanceOf[Long]).longValue())
		print(")")
	    } else if (value.isInstanceOf[Float]) {
        print(msilSyntaxFloat(value.asInstanceOf[Float]))
	    } else if (value.isInstanceOf[Double]) {
        print(msilSyntaxDouble(value.asInstanceOf[Double]))
	    } else {
		throw new Error("ILPrinterVisitor: Illegal default value: "
				+ value.getClass())
	    }
	}
	println()
        printAttributes(field)
    }

    def msilSyntaxFloat(valFlo: java.lang.Float) : String = {
      // !!! check if encoding is correct
      val bits = java.lang.Float.floatToRawIntBits(valFlo.floatValue())
      /* see p. 170 in Lidin's book Expert .NET 2.0 IL Assembler */
      /* Note: no value is equal to Nan, including NaN. Thus, x == Float.NaN always evaluates to false. */
      val res = if (valFlo.isNaN) "0xFFC00000 /* NaN */ " /* TODO this is 'quiet NaN, http://www.savrola.com/resources/NaN.html , what's the difference with a 'signaling NaN'?? */
                else if (java.lang.Float.NEGATIVE_INFINITY == valFlo.floatValue) "0xFF800000 /* NEGATIVE_INFINITY */ "
                else if (java.lang.Float.POSITIVE_INFINITY == valFlo.floatValue) "0x7F800000 /* POSITIVE_INFINITY */ "
                else bits
      "float32 (" + res + ")"
    }

  def msilSyntaxDouble(valDou: java.lang.Double) : String = {
    // !!! check if encoding is correct
    val bits = java.lang.Double.doubleToRawLongBits(valDou.doubleValue())
    /* see p. 170 in Lidin's book Expert .NET 2.0 IL Assembler */
    /* Note: no value is equal to Nan, including NaN. Thus, x == Double.NaN always evaluates to false. */
    val res = if (valDou.isNaN) "0xffffffffffffffff /* NaN */ " /* TODO this is 'quiet NaN, http://www.savrola.com/resources/NaN.html , what's the difference with a 'signaling NaN'?? */
              else if (java.lang.Double.NEGATIVE_INFINITY == valDou.doubleValue) "0xfff0000000000000 /* NEGATIVE_INFINITY */ "
              else if (java.lang.Double.POSITIVE_INFINITY == valDou.doubleValue) "0x7ff0000000000000 /* POSITIVE_INFINITY */ "
              else bits
    // float64(float64(...)) != float64(...)
    "float64 (" + res + ")"
  }

    /**
     * Visit a ConstructorBuilder
     */
    @throws(classOf[IOException])
    def caseConstructorBuilder(constr: ConstructorBuilder) {
        if (nomembers) return
	print(".method "); printHeader(constr, VOID)
	println(); println("{"); indent()
        printAttributes(constr)
	try {
	    print(constr.GetILGenerator())
	} catch {
        case e : RuntimeException => {
	       System.err.println("In method " + constr)
	       e.printStackTrace()
        }
	}
	undent(); println("}")
    }

    /**
     * Visit a MethodBuilder
     */
    @throws(classOf[IOException])
    def caseMethodBuilder(method: MethodBuilder) {
        if (nomembers) return
	print(".method "); printHeader(method, method.ReturnType)
	if (method.IsAbstract()
	    || (method.DeclaringType != null
		&& method.DeclaringType.IsInterface()
		&& !method.IsStatic()))
	    {
		println(" {"); indent()
                printAttributes(method)
                undent(); println("}")
	    } else {
		println(); println("{"); indent()
                printAttributes(method)
		if (method == entryPoint)
		    println(".entrypoint")
		try {
		    print(method.GetILGenerator())
		} catch {
            case e: RuntimeException =>
		      System.err.println("In method " + method)
		      e.printStackTrace()
		}
		undent(); println("}")
	    }
    }

    /**
     * Visit a ParameterBuilder
     */
    @throws(classOf[IOException])
    def caseParameterBuilder(param: ParameterBuilder) {
	print(ParameterAttributes.toString(param.Attributes))
	printSignature(param.ParameterType)
	//print(' ') print(marshal)
	print(' '); printName(param.Name)
    }

  var locals: Array[LocalBuilder] = null
    /**
     * Visit an ILGenerator
     */
    @throws(classOf[IOException])
    def caseILGenerator(code: ILGenerator) {
	// print maxstack
	println(".maxstack   " + code.getMaxStacksize())
	// get the local variables
	locals = code.getLocals()
	if (locals.length > 0) {
	    println(".locals init (")
	    indent()
        for (i <- 0 until locals.length) {
		if (i > 0) println(",")
		print(locals(i))
	    } // end while
	    undent()
	    println(")")
	}
	// get 3 iterators for the 3 lists
	val itL = code.getLabelIterator()
	val itO = code.getOpcodeIterator()
	val itA = code.getArgumentIterator()
	// iterate over each opcode
	while (itO.hasNext) {
	    // first print label
	    val label = itL.next
      val oOpt = code.lineNums.get(label)
      if (oOpt.isDefined) {
        println(".line       " + oOpt.get)
      }
	    argument = itA.next.asInstanceOf[Object]
	    printLabel(label)
            val o2 = itO.next
            if (o2 != null) {
                print("   ")
                print(o2.asInstanceOf[OpCode])
            }
	    println()
	} // end while
    }

    /**
     * visit an OpCode
     */
    @throws(classOf[IOException])
    def caseOpCode(opCode: OpCode) {
        val opString = opCode.toString()
	print(opString)
	pad(14 - opString.length())

	// switch opcode
        if (opCode == OpCode.Ldstr) {
            print(msilString(argument.toString()))
        } else if(opCode == OpCode.Switch) {
	    // switch ( <labels> )
	    print("(")
	    val targets = argument.asInstanceOf[Array[Label]]
            val m = targets.length
            for (i <- 0 until m) {
	        if (i != 0) print(", ")
		print(targets(i))
	    } // end for
	    print(")")
        } else if(opCode == OpCode.Call || opCode == OpCode.Callvirt || opCode == OpCode.Jmp || opCode == OpCode.Ldftn || opCode == OpCode.Ldvirtftn) {
        // call  | callvirt | jmp | ldftn | ldvirtftn
        // <instr_method> <callConv> <type> [ <typeSpec> :: ] <methodName>
	    printSignature(argument.asInstanceOf[MethodBase])
        } else if (opCode == OpCode.Newobj) {
	    printSignature(argument.asInstanceOf[ConstructorInfo])
    // ldfld | ldflda | ldsfld | ldsflda | stfld | stsfld
        } else if (opCode == OpCode.Ldfld || opCode == OpCode.Ldflda || opCode == OpCode.Ldsfld || opCode == OpCode.Ldsflda || opCode == OpCode.Stfld || opCode == OpCode.Stsfld) {
	    printSignature(argument.asInstanceOf[FieldInfo])
        } else if (opCode == OpCode.Castclass || opCode == OpCode.Isinst || opCode == OpCode.Ldobj || opCode == OpCode.Newarr) {
	    printSignature(argument.asInstanceOf[Type])
    } else if (opCode == OpCode.Box || opCode == OpCode.Unbox || opCode == OpCode.Ldtoken || opCode == OpCode.Initobj) {
	    printReference(argument.asInstanceOf[Type])
        } else if (opCode == OpCode.Ldloc || opCode == OpCode.Ldloc_S || opCode == OpCode.Ldloca || opCode == OpCode.Ldloca_S || opCode == OpCode.Stloc || opCode == OpCode.Stloc_S) {
	    val loc = argument.asInstanceOf[LocalBuilder]
	    print(loc.slot); print("\t// "); printSignature(loc.LocalType)
	    print(" \'"); print(loc.name); print("\'")
	    //print("'") print(((LocalBuilder)argument).name) print("'")
    } else if (opCode == OpCode.Ldloc_0 || opCode == OpCode.Ldloc_1 || opCode == OpCode.Ldloc_2 || opCode == OpCode.Ldloc_3 ) {
          val loc = locals(opCode.CEE_opcode - OpCode.CEE_LDLOC_0)
          print("\t// "); printSignature(loc.LocalType)
          print(" \'"); print(loc.name); print("\'")
    } else if (opCode == OpCode.Stloc_0 || opCode == OpCode.Stloc_1 || opCode == OpCode.Stloc_2 || opCode == OpCode.Stloc_3 ) {
          val loc = locals(opCode.CEE_opcode - OpCode.CEE_STLOC_0)
          print("\t// "); printSignature(loc.LocalType)
          print(" \'"); print(loc.name); print("\'")
    } else if (opCode == OpCode.Readonly) {
      // nothing to do
    } else if (opCode == OpCode.Constrained) {
      printReference(argument.asInstanceOf[Type])
    } else if (opCode == OpCode.Ldelema) {
      printReference(argument.asInstanceOf[Type])
        } else {
	    // by default print toString argument if any
	    if (argument != null) {
        val strArgument = java.lang.String.valueOf(argument)
        if (         argument.isInstanceOf[java.lang.Float]
                  && (   strArgument.equals("NaN")
                      || strArgument.equals("-Infinity")
                      || strArgument.equals("Infinity")))
                print(msilSyntaxFloat(argument.asInstanceOf[java.lang.Float]))
        else if (    argument.isInstanceOf[java.lang.Double]
                  && (   strArgument.equals("NaN")
                      || strArgument.equals("-Infinity")
                      || strArgument.equals("Infinity")))
                print(msilSyntaxDouble(argument.asInstanceOf[java.lang.Double]))
        else print(strArgument)
      }

	} // end switch
    }

    /**
     * Visit a Label
     */
    def printLabel(label: Label) {
       val kind = label.getKind()
       if (kind == Label.Kind.Normal) {
          print(label+ ": ")
       } else if (kind == Label.Kind.NewScope) {
          print("{"); indent()
       } else if (kind == Label.Kind.EndScope) {
	  undent(); print("}")
       } else if (kind == Label.Kind.Try) {
          print(".try {"); indent()
       } else if (kind == Label.Kind.Catch) {
	  undent()
	  println("}")
	  print("catch ")
	  printReference(argument.asInstanceOf[Type])
	  print(" {")
	  indent()
       } else if (kind == Label.Kind.Filter) {
	  undent()
	  println("}")
	  print("filter {")
	  indent()
       } else if (kind == Label.Kind.EndFilter) {
	  print("endfilter")
	  undent()
	  println("}")
       } else if (kind == Label.Kind.Finally) {
	  undent()
	  println("}")
	  print("finally {")
	  indent()
       } else if (kind == Label.Kind.EndTry) {
	  undent()
	  print("}")
       }
    }

    /**
     * Visit a LocalBuilder
     */
    @throws(classOf[IOException])
    def caseLocalBuilder(localBuilder: LocalBuilder) {
	// print type
	printSignature(localBuilder.LocalType)
	// space
	print(" \'")
	// print name
	print(localBuilder.name)
	print("\'")
    }


    //##########################################################################

    def printAssemblySignature(assem: Assembly, extern: Boolean) {
        print(".assembly ")
        if (extern)
            print("extern ")
        val an = assem.GetName()
	printName(an.Name); println()
	println("{")
        if (!extern)
            printAttributes(assem)
	val v = an.Version
	if (v != null) {
	    print("    .ver "); print(v.Major); print(':'); print(v.Minor)
	    print(':'); print(v.Build); print(':')
	    print(v.Revision); println()
	}
	var key = an.GetPublicKeyToken()
	if (key != null) {
	    print("    .publickeytoken = ("); print(PEFile.bytes2hex(key))
	    println(")")
	} else {
	    key = an.GetPublicKey()
	    if (key != null) {
		print("    .publickey = ("); print(PEFile.bytes2hex(key))
		println(")")
	    }
	}
	println("}")
    }


  def printSignature(field: FieldInfo) {
    printSignature(field.FieldType, field.cmods)
    //print(' ') print(owner)
    print(' ')
    //if (field.IsStatic && field.DeclaringType != currentType) {
    printReference(field.DeclaringType)
    print("::")
    //}
    printName(field.Name)
  }

    // print method head
    @throws(classOf[IOException])
    def printHeader(method: MethodBase, returnType: Type) {
	print(MethodAttributes.toString(method.Attributes))
	print(' '); print(CallingConventions.toString(method.CallingConvention))
	print(' '); printSignature(returnType)
	//print(' ') print(marshal)
	print(' '); printName(method.Name)
    if(method.isInstanceOf[MethodInfo]) {
      val mthdInfo = method.asInstanceOf[MethodInfo]
      printTypeParams(mthdInfo.getSortedMVars())
    }
	val params = method.GetParameters()
	print('(')
	for (i <- 0 until params.length) {
	    if (i > 0) print(", ")
	    print(params(i).asInstanceOf[ParameterBuilder])
	}
	print(") ")

	print(MethodImplAttributes
	      .toString(method.GetMethodImplementationFlags()))
    }


    def printSignature(method: MethodBase) {
	var returnType: Type = null
	if (method.isInstanceOf[MethodInfo])
	    returnType = (method.asInstanceOf[MethodInfo]).ReturnType
	else if (method.isInstanceOf[ConstructorInfo])
	    returnType = VOID
	else
	    throw new RuntimeException()

	val s = CallingConventions.toString(method.CallingConvention)
	print(s)
	if (s.length() > 0) print(' ')
	printSignature(returnType)
	//print(' ') print(owner)
	print(' '); printReference(method.DeclaringType)
	print("::"); printName(method.Name)

        val params = method.GetParameters()
	print("(")
	for (i <- 0 until params.length) {
	    if (i > 0) print(", ")
	    printSignature(params(i).ParameterType)
	}
	print(")")
    }

  def printSignature(marked: Type, cmods: Array[CustomModifier]) {
    printSignature(marked)
    if( (cmods != null) && !cmods.isEmpty ) {
      print(" ")
      for(cm <- cmods) {
        print(if (cm.isReqd) "modreq( " else "modopt( ")
        printReference(cm.marker)
        print(" ) ")
      }
    }
  }

  def printSignature(`type`: Type) {
      val sigOpt = primitive.get(`type`)
      if (sigOpt.isDefined) {
          print(sigOpt.get)
	    return
	}
	if (`type`.HasElementType()) {
	    printSignature(`type`.GetElementType())
	    if (`type`.IsArray())
		print("[]")
	    else if (`type`.IsPointer())
		print('*')
	    else if (`type`.IsByRef())
		print('&')
	} else {
          val preref = if (`type`.isInstanceOf[Type.TMVarUsage]) ""
                       else if(`type`.IsValueType()) "valuetype "
                       else "class "
          print(preref)
	    printReference(`type`)
	}
    }

    def printReference(`type`: Type) {
      if (`type`.Module != null) { // i.e. not PrimitiveType and not TMVarUsage
	if (`type`.Assembly() != currentModule.Assembly) {
	    print('['); print(`type`.Assembly().GetName().Name); print("]")
	} else if (`type`.Module != currentModule) {
	    print("[.module "); print(`type`.Module.Name); print("]")
	}
      }
	printTypeName(`type`)
    }

    def printTypeName(`type`: Type) {
    if (`type`.isInstanceOf[ConstructedType]) {
      val ct = `type`.asInstanceOf[ConstructedType]
        printTypeName(ct.instantiatedType)
      print("<")
      var i = 0
      while (i < ct.typeArgs.length) {
        val ta = ct.typeArgs(i)
          val sigOpt = primitive.get(ta)
          if (sigOpt.isDefined) print(sigOpt.get)
          else printTypeName(ta); /* should be printSignature, but don't want `class` or `valuetype`
        appearing before a type param usage. */
        i = i + 1;
        if (i < ct.typeArgs.length) {
          print(", ")
        }
      }
      print(">")
    } else if (`type`.DeclaringType != null) {
	    printTypeName(`type`.DeclaringType)
	    print('/')
	    printName(`type`.Name)
	} else {
	    printName(`type`.FullName)
    }
    }

    def printAttributes(icap: ICustomAttributeProvider) {
        val attrs = icap.GetCustomAttributes(false)
        for (i <- 0 until attrs.length) {
            print(".custom ")
            printSignature((attrs(i).asInstanceOf[Attribute]).getConstructor())
            print(" = (")
            print(PEFile.bytes2hex((attrs(i).asInstanceOf[Attribute]).getValue()))
            println(")")
        }
    }

    //##########################################################################

}  // class ILPrinterVisitor

object ILPrinterVisitor {
    final val VOID: Type = Type.GetType("System.Void")
    protected final val TAB = 4

    protected final val SPACES = "                                "
    protected final val SPACES_LEN = SPACES.length()

    def hasControlChars(str: String): Boolean = {
    for(i <- 0 until str.length()) {
        val ch = str.charAt(i)
        ch match {
          case '\b' =>
          case '\t' =>
          case '\n' =>
          case '\f' =>
          case '\r' =>
          case _    => if(Character.isISOControl(ch)) return true
        }
    }
    return false
    }

    final val EMPTY: String = ""
    def msilString(s: String): String = {
    if (hasControlChars(s)) {
        try {
        return "bytearray (" + PEFile.bytes2hex(s.getBytes("UTF-16LE")) + ")"
        } catch {
          case e : java.io.UnsupportedEncodingException => throw new RuntimeException(e)
        }
    }
    val str = new StringBuffer(s)
    var ss = EMPTY
    var i = 0
    while(i < str.length()) {
        ss = EMPTY
        val c = str.charAt(i)
        c match {
          case '\b' => ss = "\\b"
          case '\t' => ss = "\\t"
          case '\n' => ss = "\\n"
          case '\f' => ss = "\\f"
          case '\r' => ss = "\\r"
          case '\"' => ss = "\\\""
          case '\'' => ss = "\\\'"
          case '\\' => ss = "\\\\"
          case  _   => if (Character.isISOControl(c))
                         ss = "\\u" + PEFile.int2hex(Character.getNumericValue(c))
        }
        if (ss != EMPTY) {
        str.replace(i, i + 1, ss)
        i = i + ss.length() - 1
        }
        i = i + 1
    }
    return "\"" + str.toString() + "\""
    }

    /**
     * the main printer method
     */
    @throws(classOf[IOException])
    def printAssembly(assemblyBuilder: AssemblyBuilder, fileName: String) {
      assemblyBuilder.apply(new SingleFileILPrinterVisitor(fileName))
    }

    @throws(classOf[IOException])
    def printAssembly(assemblyBuilder: AssemblyBuilder, destPath: String, sourceFilesPath: String) {
      assemblyBuilder.apply(new MultipleFilesILPrinterVisitor(destPath, sourceFilesPath))
    }

    /** The current assembly */
    var currAssembly: Assembly = _

    final var primitive = scala.collection.mutable.Map.empty[Type, String]
    def addPrimitive(name: String, sig: String) {
      val `type` =
      Type.GetType(name)
      assert(`type` != null, "Cannot lookup primitive type " + `type`)
      primitive.put(`type`, sig)
    }

    addPrimitive("System.Object", "object")
    addPrimitive("System.String", "string")
    addPrimitive("System.Void", "void")
    addPrimitive("System.Boolean", "bool")
    addPrimitive("System.Char", "char")
    addPrimitive("System.SByte", "int8")
    addPrimitive("System.Byte", "unsigned int8")
    addPrimitive("System.Int16", "int16")
    addPrimitive("System.UInt16", "unsigned int16")
    addPrimitive("System.Int32", "int32")
    addPrimitive("System.UInt32", "unsigned int32")
    addPrimitive("System.Int64", "int64")
    addPrimitive("System.UInt64", "unsigned int64")
    addPrimitive("System.IntPtr", "native int")
    addPrimitive("System.UIntPtr", "unsigned native int")
    addPrimitive("System.Single", "float32")
    addPrimitive("System.Double", "float64")
    addPrimitive("System.TypedReference", "typedref")
}
