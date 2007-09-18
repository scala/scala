package scala.tools.nsc.symtab;
import scala.tools.nsc.io.{PlainFile,AbstractFile}
import scala.tools.nsc.util._
import java.io._
import java.util.zip._
import scala.collection.jcl.{LinkedHashMap,LinkedHashSet}
import scala.tools.nsc.symtab.Flags._

abstract class GenerateIdeMaps extends SubComponent {
  val phaseName = "gen-ide-map"
  def newPhase(prev: Phase): StdPhase = new GenIdeMapPhase(prev)
  import global._
  private def assert(b : Boolean) : Unit = {
    if (!b) {
      assert(true)
      throw new AssertionError
    }
  }

  object sourceFiles extends LinkedHashMap[AbstractFile,AbstractFile] {
    override def update(sourceFile : AbstractFile, classDir : AbstractFile) = {
      ideMaps.get(sourceFile) match {
      case Some(map) if map.isEmpty => ideMaps.removeKey(sourceFile)
      case _ =>
      }
      super.update(sourceFile, classDir)
    }
  }
  object ideMaps extends LinkedHashMap[AbstractFile, LinkedHashMap[Int,IdeRef]]
  def getIdeMap(sourceFile : SourceFile, pkg : => Option[String], defaultClassDir : => Option[AbstractFile]) = ideMaps.get(sourceFile.file) match {
  case Some(map) if !map.isEmpty => map
  case _ =>
    val ret0 = try {
      readIdeMap(sourceFile, pkg, defaultClassDir)
    } catch {
      case _ => Nil
    }
    val ret = new LinkedHashMap[Int,IdeRef]
    ret0.foreach{
    case (offset,ref) => ret(offset) = ref
    }
    //if (!ret.isEmpty)
    ideMaps(sourceFile.file) = ret
    ret
  }
  def symToPackage(sym : Symbol) : String = {
    var pkg = sym.fullNameString
    if (pkg.indexOf('$') != -1) pkg = pkg.substring(pkg.indexOf('$'))
    if (pkg.indexOf('.') != -1) pkg = pkg.substring(pkg.lastIndexOf('.'))
    pkg
  }

  abstract class IdeRef
  object NoRef extends IdeRef
  case class Internal(offset : Int) extends IdeRef
  case class External(sym : Symbol) extends IdeRef
  class Definition(sym : Symbol) extends External(sym) {
    override def toString = "Definition(" + sym + ")"
  }
  def readIdeMap(sourceFile : SourceFile, pkg : => Option[String], defaultClassDir : => Option[AbstractFile]) : List[(Int,IdeRef)] = {
    // need to find the place where the class file was loaded....
    var name = sourceFile.file.name
    assert(name.endsWith(".scala"))
    name = name.substring(0, name.length - (".scala").length) + ".ide"
    val classDir= sourceFiles.get(sourceFile.file) orElse ((defaultClassDir,pkg) match {
      case (Some(root),Some(pkg)) =>
        var file = root
        pkg.split('.').foreach{name =>
          if (file != null) file = file.lookupName(name, true)
        }
        if (file != null) Some(file) else None
      case _ => None
    })
    if (classDir.isEmpty) return Nil
    val ideFile = classDir.get.lookupPath(name, false)
    if (ideFile == null) return Nil
    try {
      val fis = new ByteArrayInputStream(ideFile.toByteArray)
      val gis = new GZIPInputStream(fis)
      val array = new Array[Byte](100)
      val buf = new scala.collection.mutable.ArrayBuffer[Byte]
      while ({
        val read = gis.read(array)
        if (read == -1) false else {
          buf ++= array.projection.take(read)
          true
        }
      }) {}
      val input = new ObjectInputStream(new ByteArrayInputStream(buf.toArray))
      def read : List[(Int,String)] = {
        val array0 = input.readObject.asInstanceOf[Array[Int]]
        val array1 = input.readObject.asInstanceOf[Array[String]]
        assert(array0.length == array1.length)
        val buf = new ListBuffer[(Int,String)]
        val i = array0.elements
        val j = array1.elements
        while (i.hasNext) buf += (i.next,j.next)
        buf.toList
      }
      val uses = read
      val defs = read
      val defs0 = new LinkedHashMap[Int,Symbol]
      val ret = new scala.collection.jcl.LinkedHashMap[Int,IdeRef]
      implicit val cache = new LinkedHashMap[String,Symbol]
      defs.foreach{
      case (offset,url) =>
        val sym = url2sym(url)
        //assert(sym != null)
        if (sym != null) {
          sym.setPos(new OffsetPosition(sourceFile, offset))
          ret += ((offset,new Definition(sym)))
          defs0(offset) = sym
        } else {
          assert(true)
          assert(true)
        }
        if (sym != null) {
        }
      }
      uses.foreach{
      case (offset,url) =>
        if (url.startsWith(":")) {
          val offset0 = Integer.parseInt(url.substring(1))
          ret(offset) = (defs0.get(offset0) match {
          case Some(sym) => new Definition(sym)
          case None => Internal(offset0)
          })
        } else {
          val sym = url2sym(url)
          if (sym != null) {
            ret(offset) = (External(sym))
          } else {
            assert(true)
            assert(true)
          }
        }
      }
      ret.toList
    } catch {
      case ex =>
        //ex.printStackTrace
        Nil
    }
  }

  def decodeIdeName(string : String) : (Name,String) = {
    val idx = string.indexOf('[')
    if (idx != -1) { // a method
      return (newTermName(string.substring(0, idx)), string.substring(idx))
    } else if (string.endsWith(";")) {
      return (newTermName(string.substring(0, string.length - (";").length)), null)
    } else return (newTypeName(string), null)
  }
  def lookupIdeName(owner : Symbol, scope : Scope, string : String) : Symbol = {
    val (name,params) = decodeIdeName(string)
    var e = scope.lookupEntry(name)
    while ((e ne null) && params != null && methodSig(e.sym.info) != params)
      e = scope.lookupNextEntry(e)
    if (e != null) return (e.sym)
    val isPC = owner.isPackageClass
    if (isPC) {
      assert(params == null)
      assert(true)
      val info = owner.info.asInstanceOf[PackageClassInfoType]
      val loader = info.loader.asInstanceOf[global.loaders.PackageLoader]
      if (loader ne null) loader.refresh
      e = scope.lookupEntry(name)
      if (e ne null) return (e.sym)
    }
    assert(true) // not found!
    return null
  }
  def walk(sourceFile : SourceFile, tree: Tree, uses : scala.collection.mutable.Map[Position,Symbol], defs : LinkedHashMap[Symbol,Position]) : Unit = {
    def f(t : Tree) : Unit = {
      def fs(l : List[Tree]) : Unit = {
        val i = l.elements
        while (i.hasNext) f(i.next)
      }
      def fss(l : List[List[Tree]]) : Unit = {
        val i = l.elements
        while (i.hasNext) fs(i.next)
      }
      if (t.isInstanceOf[StubTree]) return
      def asTypeRef = t.tpe.asInstanceOf[TypeRef]
      val sym = (t,t.tpe) match {
        case (Super(_,_),SuperType(_,supertp)) if supertp.typeSymbol != NoSymbol && supertp.typeSymbol != null => supertp.typeSymbol
        case _ if t.symbol != NoSymbol && t.symbol != null => t.symbol
        case (t : TypeTree, tp) if tp != null && tp.typeSymbol != null && tp.typeSymbol != NoSymbol => tp.typeSymbol
        case (t : TypeTree, tp) if tp != null && tp.resultType != null && tp.resultType.typeSymbol != null => tp.resultType.typeSymbol
        case (t, tpe : Type) if tpe != null && (t.symbol eq NoSymbol) && t.isTerm && tpe.termSymbol != null =>
          assert(true)
          tpe.termSymbol
        case (t, tpe : Type) if tpe != null && (t.symbol eq NoSymbol) && tpe.typeSymbol != null =>
    	  if (t.tpe.isInstanceOf[TypeRef]) asTypeRef.sym // XXX: looks like a bug
   	  else tpe.typeSymbol
        case _ => NoSymbol
      }
      if (sym != null && sym != NoSymbol /* && !sym.hasFlag(SYNTHETIC) */) {
        var id = sourceFile.identifier(t.pos, global)
        val doAdd = if (id.isDefined) {
          if (id.get.charAt(0) == '`') id = Some(id.get.substring(1, id.get.length - 1))
          val name = sym.name.decode.trim
          if (name == id.get) true
          else if (name.endsWith("_=")) {
            val name1 = name.substring(0, name.length - ("_=").length)
            name1 == id.get
          } else false
          //if (sym.name.toString.contains(id.get)) true else false
        } else false
        if (doAdd) {

        if (!uses.contains(t.pos)) {
          uses(t.pos) = sym
        } else {
          val existing = uses(t.pos)
          if (sym.sourceFile != existing.sourceFile || sym.pos != existing.pos) {
            (sym,existing) match {
            case (sym,existing) if sym.pos == existing.pos =>
            case (sym : TypeSymbol ,_ : ClassSymbol) => uses(t.pos) = sym
            case (_ : ClassSymbol,_ : TypeSymbol) => // nothing
            case _ if sym.isModule && existing.isValue => // nothing
            case _ if sym.isClass && existing.isMethod => // nothing
            case _ =>
              assert(true)
            }
          }
        }}
      }
      assert(true)
      t match {
      case t : DefTree if defs != null && t.symbol != NoSymbol =>
        if (t.pos != NoPosition)
          defs.put(t.symbol, t.pos)
          if (t.symbol.isClass) {
            val factory = t.symbol.caseFactory
            if (factory != NoSymbol) {
              assert(true)
              defs.put(factory, t.pos)
            }
          }
        else {
          assert(true)
          assert(true)
        }
      case t : TypeBoundsTree => f(t.lo); f(t.hi)
      case t : TypeTree if t.original != null =>
        def h(original : Tree, tpe : Type): Unit = try {
          if (original.tpe == null)
            original.tpe = tpe
          (original) match {
          case (AppliedTypeTree(_,trees)) if tpe.isInstanceOf[TypeRef] =>
            val types = tpe.asInstanceOf[TypeRef].args
            trees.zip(types).foreach{
            case (tree,tpe) => assert(tree != null && tpe != null); h(tree, tpe)
            case _ =>
            }
          case _ =>
          }
        }
        if (t.original.tpe == null) {
          val dup = t.original.duplicate
          h(dup,t.tpe)
          f(dup)
        } else f(t.original)
        ()
      case _ =>
      }
      (t) match {
      case (t : MemberDef) if t.symbol != null && t.symbol != NoSymbol =>
        val annotated = if (sym.isModule) sym.moduleClass else sym
        val i = t.mods.annotations.elements
        val j = annotated.attributes.elements
        while (i.hasNext && j.hasNext) {
          val tree = i.next.constr
          val ainfo = j.next
          val sym = ainfo.atp.typeSymbol
          tree.setType(ainfo.atp)
          tree.setSymbol(sym)
          f(tree)
        }

      case _ =>
      }
      t match {
      case tree: ImplDef =>
        fs(tree.impl.parents); f(tree.impl.self); fs(tree.impl.body)
        tree match {
        case tree : ClassDef => fs(tree.tparams)
        case _ =>
        }
      case tree: PackageDef => fs(tree.stats)
      case tree: ValOrDefDef =>
        f(tree.rhs);
        if (tree.tpt != null) {
          assert(true)
          f(tree.tpt)
        }
        tree match {
        case tree : DefDef => fs(tree.tparams); fss(tree.vparamss)
        case _ =>
        }
      case tree: Function => fs(tree.vparams); f(tree.body)
      case tree : Bind => f(tree.body)
      case tree : Select =>
        val qualifier = if (tree.tpe != null && tree.qualifier.tpe == null) {
          val pre = tree.tpe.prefix
          val qualifier = tree.qualifier.duplicate
          qualifier.tpe = pre
          qualifier
        } else tree.qualifier

        f(qualifier)
      case tree : Annotation => f(tree.constr)
      case tree : Annotated => f(tree.annot); f(tree.arg)
      case tree : GenericApply => f(tree.fun); fs(tree.args)
      case tree : UnApply => f(tree.fun); fs(tree.args)
      case tree : AppliedTypeTree =>
        if (tree.tpe != null) {
          val i = tree.tpe.typeArgs.elements
          val j = tree.args.elements
          while (i.hasNext && j.hasNext) {
            val tpe = i.next
            val arg = j.next
            if (arg.tpe == null) {
              arg.tpe = tpe
            }
          }
        }
        f(tree.tpt); fs(tree.args)
      case tree : SingletonTypeTree =>
        if (tree.ref.tpe == null) {
          val dup = tree.ref.duplicate
          dup.tpe = tree.tpe
          f(dup)
        } else f(tree.ref)
      case tree : SelectFromTypeTree => f(tree.qualifier)
      case tree : Typed => f(tree.expr); f(tree.tpt)
      case tree : Block => fs(tree.stats); f(tree.expr)
      case tree: CaseDef => f(tree.pat);f(tree.guard);f(tree.body)
      case tree : Sequence   => fs(tree.trees);
      case tree : Assign     => f(tree.lhs); f(tree.rhs);
      case tree : If         => f(tree.cond); f(tree.thenp); f(tree.elsep);
      case tree : New        => f(tree.tpt);
      case tree : Match      => f(tree.selector); fs(tree.cases);
      case tree : Return     => f(tree.expr);
      case tree : LabelDef   => f(tree.rhs);
      case tree : Throw      => f(tree.expr);
      case tree : Try        => f(tree.block); fs(tree.catches); f(tree.finalizer);
      case tree : Alternative => fs(tree.trees);
      case tree : TypeDef => f(tree.rhs); fs(tree.tparams)
      case tree : DocDef     => f(tree.definition);
      case tree: Import => f(tree.expr)
      case _ =>
      }
    }
    f(tree)
  }
  def isRoot(sym : Symbol) = sym == definitions.RootPackage || sym == definitions.RootClass || sym == NoSymbol
  private var depth = 0

  def methodSig(info : Type) : String = info match {
  case PolyType(_, result) => methodSig(result)
  case info =>
    info.paramTypes.map{t =>
    if (depth >= 10) {
      assert(true)
      assert(true)
    }
    assert(!isRoot(t.typeSymbol))
    //assert(t.symbol != sym && t.symbol != sym.owner)
    depth += 1
    val ret = sym2url(t.typeSymbol) +
    (t match {
    case TypeRef(_,_,tparams) if !tparams.isEmpty =>
      tparams.map{t => sym2url(t.typeSymbol)}.mkString("<",":",">")
    case _ => ""
    })
    depth -= 1
    ret
    }.mkString("[",":","]") + (info.resultType match {
    case MethodType(args,result) => "*"
    case tpe => sym2url(tpe.typeSymbol)
    })
  }

  def sym2url(sym : Symbol) : String = {
    assert(!isRoot(sym))
    assert(sym != sym.owner)
    if (sym.isModuleClass) {
      if (sym.linkedModuleOfClass eq NoSymbol) {
        if (isRoot(sym.owner)) {
          return sym.name.toString
        }
        throw new Error(sym + " " + sym.moduleClass + " " + sym.isPackage + " " + sym.owner)
      }
      return sym2url(sym.linkedModuleOfClass)
    }
    (if (isRoot(sym.owner) || sym.owner.isTerm) ""
     else sym2url(sym.owner) + ".") + sym.name.toString + {
      //assert(!sym.owner.isTerm)
      if (sym.isMethod) methodSig(sym.info)
      else if (!sym.name.isTypeName) ";" else ""
    }
  }
  def url2sym(paths : List[String], owner : Symbol) : Symbol = {
    if (paths.isEmpty) owner
    else {
      // decode name
      val owner0 = if (owner.isModule) {
        assert(owner.moduleClass ne NoSymbol)
        owner.moduleClass
      } else owner

      val sym = lookupIdeName(owner0, owner0.info.decls, paths.head)
      if (sym == null) {
        assert(true)
        return null
      }
      else url2sym(paths.tail, sym)
    }
  }
  def url2sym(url : String)(implicit cache : LinkedHashMap[String,Symbol]) : Symbol = {
    if (cache != null) cache.get(url) match {
    case Some(sym) => return sym
    case _ =>
    }


    // decode the string.
    var url0 = url
    var idx = url.lastIndexOf('[') // could be method
    val paths = new scala.collection.mutable.ListBuffer[String]
    while (url0 ne null) {
      val jdx = url0.indexOf('.')
      if (jdx == -1 || (idx != -1 && jdx > idx)) {
        paths append url0
        url0 = null
      } else {
        paths append url0.substring(0, jdx)
        if (idx != -1) idx = idx - (jdx + 1)
        url0 = url0.substring(jdx + 1)
      }
    }
    if (definitions.RootClass eq null) new global.Run
    val result = url2sym(paths.toList, definitions.RootClass)
    if (result != null && cache != null) cache(url) = result
    return result
  }
  class GenIdeMapPhase(prev: Phase) extends StdPhase(prev) {
    def apply(unit: global.CompilationUnit): Unit = {
      if (global.settings.noide.value) return
      global.informProgress("gen-ide-map for " + unit)
      val path = unit.source.file match {
      case file : PlainFile => file.path
      case _ => return // don't do anything.
      }
      assert(path.endsWith(".scala"))

      val sources = global.settings.sourcepath.value.split(File.pathSeparator)
      val i = sources.elements
      while (i.hasNext) {
        val sourcePath = i.next
        if (path.startsWith(sourcePath)) {
          val usePath = path.substring(sourcePath.length, path.length - (".scala").length) + ".ide"
          val uses = new LinkedHashMap[Position,Symbol]
          val defs = new LinkedHashMap[Symbol,Position]
          walk(unit.source, unit.body, uses, defs)
          // all populated, now what?
          // populate ownerTree

          val writeUses = uses.projection.filter(_._1 != NoPosition).map{
          case (p : OffsetPosition, sym) =>
            (p.offset0, if (sym.sourceFile == unit.source.file) {
              // internal reference
              if (!sym.pos.offset.isDefined) scala.Console.println("ERROR: " + sym + " in " + sym.sourceFile)
              ":" + sym.pos.offset.getOrElse(0)
            } else {
              val url = sym2url(sym)
              url
            })
          }.toList
          object okDefs extends LinkedHashMap[Symbol,Boolean] {
            def has(sym : Symbol) : Boolean = get(sym) match {
            case Some(value) => value
            case _ =>
              if (sym eq definitions.RootClass) return true
              if (sym.isModuleClass) {
                if (sym.linkedModuleOfClass ne NoSymbol)
                  return has(sym.linkedModuleOfClass)
                //System.err.println(sym + " is module class")
                return false
              }
              val owner = sym.owner
              if (owner eq NoSymbol) return false
              if (!owner.isClass && !owner.isStable) return false
              def fail = {
                put(sym, false)
                false
              }
              if (!has(owner)) return fail
              var e = owner.info.decls.lookupEntry(sym.name)
              while (e != null&&e.sym != sym) e = owner.info.decls.lookupNextEntry(e)
              if (e == null) return fail
              put(sym, true)
              return true
            }
          }

          val writeDefs = defs.projection.filter{
          case (sym,OffsetPosition(_,_)) if okDefs.has(sym) => true
          case _ => false
          }.map{
          case (sym,OffsetPosition(source,offset)) if unit.source == source => (offset,sym2url(sym))
          }.toList

          val file = new File(global.settings.outdir.value, usePath)
          file.mkdirs
          if (file.exists) file.delete

          val out = new ObjectOutputStream(new GZIPOutputStream(new FileOutputStream(file)))
          //Console.println("GZIP: " + out)
          def write(list : List[(Int,String)]) = {
            val array0 = list.projection.map(_._1).toArray
            val array1 = list.projection.map(_._2).toArray
            out.writeObject(array0)
            out.writeObject(array1)
          }
          write(writeUses)
          write(writeDefs)
          out.close
          ideMaps.removeKey(unit.source.file)
          return
        }
      }
      //Console.println("Cannot find root source of " + path + " so cannot generate map file")
    }
  }
  case class ProjectAdapter(other : Global) {
    def translate(sym : global.Symbol) : other.Symbol = {
      assert(sym != NoSymbol)
      val from = sym2url(sym)
      // ya, its this easy!
      return other.generateIdeMaps.url2sym(from)(null)
    }
  }
}
