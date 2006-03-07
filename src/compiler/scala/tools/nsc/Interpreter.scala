/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc

import reporters.Reporter
import nsc.util.SourceFile
import scala.tools.util.PlainFile
import java.io.{File, Writer, PrintWriter, StringWriter}
import nsc.ast.parser.SyntaxAnalyzer
import scala.collection.mutable.{ListBuffer, HashSet, ArrayBuffer}
import scala.collection.immutable.{Map, ListMap}
import symtab.Flags

/** An interpreter for Scala code.

    The main public entry points are compile() and interpret().  The compile()
    method loads a complete Scala file.  The interpret() method executes one
    line of Scala code at the request of the user.

    The overall approach is based on compiling the requested code and then
    using a Java classloader and Java reflection to run the code
    and access its results.

    In more detail, a single compiler instance is used
    to accumulate all successfully compiled or interpreted Scala code.  To
    "interpret" a line of code, the compiler generates a fresh object that
    includes the line of code and which has public member(s) to export
    all variables defined by that code.  To extract the result of an
    interpreted line to show the user, a second "result object" is created
    which exports a single member named "result".  To accomodate user expressions
    that read from variables or methods defined in previous statements, "import"
    statements are used.

    This interpreter shares the strengths and weaknesses of using the
    full compiler-to-Java.  The main strength is that interpreted code
    behaves exactly as does compiled code, including running at full speed.
    The main weakness is that redefining classes and methods is not handled
    properly, because rebinding at the Java level is technically difficult.
*/
class Interpreter(val compiler: Global, output: (String => Unit)) {
  import symtab.Names
  import compiler.Traverser
  import compiler.{Tree, TermTree,
                   ValOrDefDef, ValDef, DefDef, Assign,
                   ClassDef, ModuleDef, Ident, Select, AliasTypeDef}
  import compiler.CompilationUnit
  import compiler.Symbol
	import compiler.Name

  /** construct an interpreter that prints to the compiler's reporter */
  def this(compiler: Global) = {
    this(compiler, str: String => compiler.reporter.info(null, str, true))
  }

  private def reporter = compiler.reporter

  /** whether to print out result lines */
  private var printResults: Boolean = true

  /** be quiet; do not print out the results of each submitted command */
  def beQuiet = { printResults = false }

  /** directory to save .class files to */
  private val classfilePath = File.createTempFile("scalaint", "")
  classfilePath.delete  // the file is created as a file; make it a directory
  classfilePath.mkdirs



  /* set up the compiler's output directory */
  compiler.settings.outdir.value = classfilePath.getPath

  /** class loader used to load compiled code */
  /* A single class loader is used for all commands interpreted by this Interpreter.
     It would also be possible to create a new class loader for each command
     to interpret.  The advantages of the current approach are:

       - Expressions are only evaluated one time.  This is especially
         significant for I/O, e.g. "val x = Console.readLine"

     The main disadvantage is:

       - Objects, classes, and methods cannot be rebound.  Instead, definitions
         shadow the old ones, and old code objects to refer to the old
         definitions.
  */
  private val classLoader = new java.net.URLClassLoader(Predef.Array(classfilePath.toURL))


  /** the previous requests this interpreter has processed */
  private val prevRequests = new ArrayBuffer[Request]()

  /** look up the request that bound a specified term or type */
  private def reqBinding(vname: Name): Option[Request] = {
    prevRequests.toList.reverse.find(lin => lin.boundNames.contains(vname))
  }

  /** next line number to use */
  var nextLineNo = 0

  /** allocate a fresh line name */
  def newLineName = {
    val num = nextLineNo
    nextLineNo = nextLineNo + 1
    "line" + num
  }

  /** generate a string using a routine that wants to write on a stream */
  private def stringFrom(writer: PrintWriter=>Unit): String = {
    val stringWriter = new StringWriter()
    val stream = new PrintWriter(stringWriter)
    writer(stream)
    stream.close
    stringWriter.toString
  }

  /** parse a line into a sequence of trees */
  private def parse(line: String): List[Tree] = {
    reporter.reset

    val unit =
      new CompilationUnit(
        new SourceFile("<console>",line.toCharArray()))

    val trees = new compiler.syntaxAnalyzer.Parser(unit).templateStatSeq

    if(reporter.errors > 0)
      return Nil // the result did not parse, so stop

    trees
  }


  /** Compile one source file */
  def compile(filename: String): Unit = {
    val jfile = new File(filename)
    if(!jfile.exists) {
      reporter.error(null, "no such file: " + filename)
      return ()
    }
    val cr = new compiler.Run
    cr.compileSources(List(new SourceFile(PlainFile.fromFile(jfile))))
  }

  /** build a request from the user.  "tree" is "line" after being parsed */
  private def buildRequest(trees: List[Tree], line: String,  lineName: String): Request = {
    trees match {
      /* This case for assignments is more specialized than desirable: it only
         handles assignments to an identifier.  It would be better to support
         arbitrary paths being assigned, but that is technically difficult
         because of the way objectSourceCode and resultObjectSourceCode are
         implemented in class Request. */
      case List(Assign(Ident(lhs), _)) => new AssignReq(lhs, line, lineName)


      case _ if trees.forall(t => t.isInstanceOf[ValOrDefDef]) => new DefReq(line, lineName)
      case List(_:TermTree) | List(_:Ident) | List(_:Select) => new ExprReq(line, lineName)
      case List(_:ModuleDef) => new ModuleReq(line, lineName)
      case List(_:ClassDef) => new ClassReq(line, lineName)
      case List(_:AliasTypeDef) => new TypeAliasReq(line, lineName)
      case _ => {
        reporter.error(null, "That kind of statement combination is not supported by the interpreter.")
        null
      }
    }

  }

  /** interpret one line of input.  All feedback, including parse errors
	    and evaluation results, are printed via the supplied compiler's
   	  reporter.  Values defined are available for future interpreted
	    strings. */
  def interpret(line: String): Unit = {
    // parse
		val trees = parse(line)
    if(trees.isEmpty) return ()  // parse error or empty input

    val lineName = newLineName

    // figure out what kind of request
    val req = buildRequest(trees, line, lineName)
    if(req == null) return ()  // a disallowed statement type


    if(!req.compile)
      return ()  // an error happened during compilation, e.g. a type error

    val interpreterResultString = req.loadAndRun

    if(printResults) {
      // print the result
      output(interpreterResultString)

      // print out types of functions; they are not printed in the
      // request printout
      output(req.defTypesSummary)
    }

    // book-keeping
    prevRequests += req
  }


  /** Delete a directory tree recursively.  Use with care! */
  private def deleteRecursively(path: File): Unit = {
    path match  {
      case _ if(!path.exists) => ()
      case _ if(path.isDirectory) =>
        for(val p <- path.listFiles)
          deleteRecursively(p)
        path.delete
      case _ => path.delete
    }
  }

  /** This instance is no longer needed, so release any resources
      it is using.

      Specifically, this deletes the temporary directory used for holding
      class files for this instance.  This cannot safely be done as commands
      are executed becaus of Java's demand loading.
  */
  def close: Unit = {
    deleteRecursively(classfilePath)
  }


  /** A traverser that finds all mentioned identifiers, i.e. things that need to be imported.
      It might return extra names.  */
  private class ImportVarsTraverser(definedVars: List[Name]) extends Traverser {
    val importVars = new HashSet[Name]()

    override def traverse(ast: Tree): unit = ast match {
      case Ident(name) => importVars += name
      case _ => super.traverse(ast)
    }
  }


  /** One line of code submitted by the user for interpretation */
  private abstract class Request(line: String, val lineName: String) {
    val trees = parse(line)

    /** name to use for the object that will compute "line" */
    def objectName = lineName + "$object"  // make it unlikely to clash with user variables

    /** name of the object that retrieves the result from the above object */
    def resultObjectName = "RequestResult$" + objectName

    /** whether the trees need a variable name, as opposed to standing
        alone */
    def needsVarName: Boolean = false

    /** list of methods defined */
		val defNames =
      for {
        val DefDef(mods, name, _, _, _, _) <- trees
        mods.isPublic
      } yield name

    /** list of val's and var's defined */
    val valAndVarNames = {
      val baseNames =
        for {
          val ValDef(mods, name, _, _) <- trees
          mods.isPublic
        } yield name

      if(needsVarName)
        compiler.encode(lineName) :: baseNames  // add a var name
      else
        baseNames
    }
      //XXXshorten all these for loops
    /** list of modules defined */
    val moduleNames = {
      val explicit =
        for(val ModuleDef(mods, name, _) <- trees; mods.isPublic)
          yield name
      val caseClasses =
        for {val ClassDef(mods, name, _, _, _) <- trees
             mods.isPublic
             mods.hasFlag(Flags.CASE)}
        yield name.toTermName
      explicit ::: caseClasses
    }

	  /** list of classes defined */
    val classNames =
      for(val ClassDef(mods, name, _, _, _) <- trees; mods.isPublic)
        yield name

    /** list of type aliases defined */
    val typeNames =
      for(val AliasTypeDef(mods, name, _, _) <- trees; mods.isPublic)
        yield name


    /** all (public) names defined by these statements */
    val boundNames = defNames ::: valAndVarNames ::: moduleNames ::: classNames ::: typeNames


    /** list of names used by this expression */
    val usedNames: List[Name] = {
      val ivt = new ImportVarsTraverser(boundNames)
      ivt.traverseTrees(trees)
      ivt.importVars.toList
    }

    /** names to print out to the user after evaluation */
    def namesToPrintForUser = valAndVarNames

    /** generate the source code for the object that computes this request */
    def objectSourceCode: String =
			stringFrom(code => {
			  // write an import for each imported variable
        for{val imv <- usedNames
            val lastDefiner <- reqBinding(imv).toList } {
         code.println("import " + lastDefiner.objectName + "." + imv)
        }

        // object header
        code.println("object "+objectName+" {")

        // the line of code to compute
        if(needsVarName)
          code.println("  val " + lineName + " = " + line)
        else
          code.println("  " + line)

        //end
        code.println(";}")
			})

    /** Types of variables defined by this request.  They are computed
        after compilation of the main object */
    var typeOf: Map[Name, String] = _


    /** generate source code for the object that retrieves the result
        from objectSourceCode */
    def resultObjectSourceCode: String =
    	stringFrom(code => {
    	  code.println("object " + resultObjectName)
        code.println("{ val result:String = {")
        code.println(objectName + ";")  // evaluate the object, to make sure its constructor is run
        code.print("\"\"")  // print an initial empty string, so later code can
                            // uniformly be: + morestuff
        resultExtractionCode(code)
        code.println("}")
        code.println(";}")
    	})

    def resultExtractionCode(code: PrintWriter): Unit = {
      for(val vname <- namesToPrintForUser) {
        code.print(" + \"" + vname + ": " + typeOf(vname) +
          " = \" + " + objectName + "." + vname + " + \"\\n\"")
      }
    }


    /** Compile the object file.  Returns whether the compilation succeeded.
        If all goes well, types is computed and set */
    def compile: Boolean = {
      reporter.reset  // without this, error counting is not correct,
                      // and the interpreter sometimes overlooks compile failures!

      // compile the main object
      val objRun = new compiler.Run()
      objRun.compileSources(List(new SourceFile("<console>", objectSourceCode.toCharArray)))
      if(reporter.errors > 0) return false

      // extract and remember types
      typeOf = findTypes(objRun)

      // compile the result-extraction object
      new compiler.Run().compileSources(List(new SourceFile("<console>", resultObjectSourceCode.toCharArray)))
      if(reporter.errors > 0) return false

      // success
      true
    }

    /** dig the types of all bound variables out of the compiler run */
    def findTypes(objRun: compiler.Run): Map[Name, String] = {
      def getTypes(names: List[Name], nameMap: Name=>Name): Map[Name, String] = {
        names.foldLeft[Map[Name,String]](new ListMap[Name, String]())((map, name) => {
          val resObjSym: Symbol =
            compiler.definitions.getMember(compiler.definitions.EmptyPackage,
              compiler.newTermName(objectName))

          val typeString =
            compiler.atPhase(objRun.typerPhase.next) {
              resObjSym.info.decls.toList.find(s=>s.name == nameMap(name)).get.tpe.toString()
            }

          map + name -> typeString
        })
      }

      val names1 = getTypes(valAndVarNames, n=>compiler.nme.getterToLocal(n))
      val names2 = getTypes(defNames, id)
      names1.incl(names2)
    }

    /** load and run the code using reflection */
    def loadAndRun: String = {
      val interpreterResultObject: Class = Class.forName(resultObjectName,true,classLoader)
      val resultValMethod: java.lang.reflect.Method = interpreterResultObject.getMethod("result",null)
      try {
	resultValMethod.invoke(interpreterResultObject,null).toString()
      } catch {
	case e => {
	  def caus(e: Throwable): Throwable =
	    if(e.getCause == null) e else caus(e.getCause)
	  val orig = caus(e)
	  stringFrom(str => orig.printStackTrace(str))
	}
      }
    }

    /** return a summary of the defined methods */
    def defTypesSummary: String =
      stringFrom(summ => {
        for(val methname <- defNames) {
          summ.println("" + methname + ": " + typeOf(methname))
        }
      })
  }

  /** A sequence of definition's.  val's, var's, def's. */
  private class DefReq(line: String, lineName: String) extends Request(line, lineName) {
  }

  /** Assignment of a single variable: lhs = exp */
  private class AssignReq(val lhs: Name, line: String, lineName: String) extends Request(line, lineName) {
    override def resultExtractionCode(code: PrintWriter): Unit = {
      super.resultExtractionCode(code)
      val bindReq = reqBinding(lhs).get
      code.println(" + \"" + lhs + " = \" + " + bindReq.objectName + "." + lhs)
    }
    override def namesToPrintForUser = Nil
  }

  /** A single expression */
  private class ExprReq(line: String, lineName: String) extends Request(line, lineName) {
    override val needsVarName = true
  }

  /** A module definition */
  private class ModuleReq(line: String, lineName: String) extends Request(line, lineName) {
    def moduleName = trees match {
      case List(ModuleDef(_, name, _)) => name
    }
    override def resultExtractionCode(code: PrintWriter): Unit = {
      super.resultExtractionCode(code)
      code.println(" + \"defined module " + moduleName + "\\n\"")
    }
  }

  /** A class definition */
  private class ClassReq(line: String, lineName: String) extends Request(line, lineName) {
    def newClassName = trees match {
      case List(ClassDef(_, name, _, _, _)) => name
    }

    override def resultExtractionCode(code: PrintWriter): Unit = {
      super.resultExtractionCode(code)
      code.println(" + \"defined class " + newClassName + "\\n\"")
    }
  }

  /** a type alias */
  private class TypeAliasReq(line: String, lineName: String) extends Request(line, lineName) {
    def newTypeName = trees match {
      case List(AliasTypeDef(_, name, _, _)) => name
    }

    override def resultExtractionCode(code: PrintWriter): Unit = {
      super.resultExtractionCode(code)
      code.println(" + \"defined type alias " + newTypeName + "\\n\"")
    }
  }
}
