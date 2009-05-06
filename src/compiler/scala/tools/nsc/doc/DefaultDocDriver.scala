/* NSC -- new Scala compiler
 * Copyright 2007-2009 LAMP/EPFL
 * @author  Sean McDirmid
 */
// $Id$

package scala.tools.nsc.doc

import java.util.zip.ZipFile

import scala.collection.jcl
import symtab.Flags._
import scala.xml._

/**
 *  @author Sean McDirmid
 */
abstract class DefaultDocDriver extends DocDriver with ModelFrames with ModelToXML {
  import global._
  import definitions.{AnyClass, AnyRefClass}

  lazy val additions = new jcl.LinkedHashSet[Symbol]
  lazy val additions0 = new ModelAdditions(global) {
    override def addition(sym: global.Symbol) = {
      super.addition(sym)
      sym match {
        case sym : global.ClassSymbol  => additions += sym.asInstanceOf[Symbol]
        case sym : global.ModuleSymbol => additions += sym.asInstanceOf[Symbol]
        case sym : global.TypeSymbol   => additions += sym.asInstanceOf[Symbol]
        case _ =>
      }
    }
    def init {}
  }

  /** Add all top-level entities in ModelAdditions to allClasses */
  def addAdditionsToClasses() {
    additions0.init
    for (sym <- additions) {
      val packSym = sym.enclosingPackage
      if (packSym != NoSymbol) {
        val pack = Package(packSym)
        if (!(allClasses contains pack)) {
          // don't emit an addition unless its package
          // is already being scaladoced
        } else {
          val addition: Option[ClassOrObject] =
            if (sym.isClass)
              Some(new TopLevelClass(sym))
            else if (sym.isModule)
              Some(new TopLevelObject(sym))
            else if (sym == definitions.AnyRefClass) {
              // AnyRef is the only top-level type alias, so handle
              // it specially instead of introducing general support for
              // top-level aliases
              Some(new TopLevelClass(sym))
            }
            else
              None

          addition match {
            case None =>
              //println("skipping: " + sym) //DEBUG
            case Some(addition) =>
              allClasses(pack) += addition
          }
        }
      } else {
        //println("no package found for: "+sym) //DEBUG
      }
    }
  }

  def process(units: Iterator[CompilationUnit]) {

    assert(global.definitions != null)

    def g(pkg: Package, clazz: ClassOrObject) {
      if (isAccessible(clazz.sym)) {
        allClasses(pkg) += clazz
        clazz.decls.map(_._2).foreach {
          case clazz : ClassOrObject => g(pkg, clazz)
          case _ =>
        }
      }
    }
    def f(pkg: Package, tree: Tree) {
      if (tree != EmptyTree && tree.hasSymbol) {
        val sym = tree.symbol
        if (sym != NoSymbol && !sym.hasFlag(symtab.Flags.PRIVATE)) tree match {
          case tree : PackageDef =>
            val pkg1 = new Package(sym.asInstanceOf[ModuleSymbol])
            tree.stats.foreach(stat => f(pkg1, stat))
          case tree : ClassDef =>
            assert(pkg != null)
            g(pkg, new TopLevelClass(sym.asInstanceOf[ClassSymbol]))
          case tree : ModuleDef =>
            assert(pkg != null)
            g(pkg, new TopLevelObject(sym.asInstanceOf[ModuleSymbol]))
          case _ =>
        }
      }
    }
    units.foreach(unit => f(null, unit.body))
    addAdditionsToClasses()

    for (p <- allClasses; d <- p._2) {
      symbols += d.sym
      for (pp <- d.sym.tpe.parents) subClasses(pp.typeSymbol) += d
    }
    copyResources
    lazy val packages0 = sort(allClasses.keySet)
    new AllPackagesFrame     with Frame { def packages = packages0 }
    new PackagesContentFrame with Frame { def packages = packages0 }
    new NavigationFrame      with Frame { }
    new ListClassFrame with Frame {
      def classes = for (p <- allClasses; d <- p._2) yield d
      object organized extends jcl.LinkedHashMap[(List[String],Boolean),List[ClassOrObject]] {
        override def default(key : (List[String],Boolean)) = Nil;
        classes.foreach(cls => {
          val path = cls.path.map(_.name);
          this((path,cls.isInstanceOf[Clazz])) = cls :: this((path,cls.isInstanceOf[Clazz]));
        });
      }

      def title = "List of all classes and objects"
      def path = "all-classes"
      def navLabel = null  // "root-page"
      // override protected def navSuffix = ".html";
      override def optional(cls: ClassOrObject): NodeSeq = {
        val path = cls.path.map(_.name)
        val key = (cls.path.map(_.name), cls.isInstanceOf[Clazz])
        assert(!organized(key).isEmpty);
        (if (!organized(key).tail.isEmpty) Text(" (" +{
          //Console.println("CONFLICT: " + path + " " + organized(key));
          val str = cls.path(0).sym.owner.fullNameString('.');
          val idx = str.lastIndexOf('.');
          if (idx == -1) str;
          else str.substring(idx + 1);
         }+ ")");
         else NodeSeq.Empty) ++ super.optional(cls);
      }

    }
    for ((pkg0, classes0) <- allClasses) {
      new ListClassFrame with Frame {
        def title =
          "List of classes and objects in package " + pkg0.fullName('.')
        def classes = classes0
        def path = pkgPath(pkg0.sym) + NAME_SUFFIX_PACKAGE
        def navLabel = pkg0.fullName('.')
      }
      new PackageContentFrame with Frame {
        def classes = classes0
        def pkg = pkg0
      }
      for (clazz0 <- classes0) {
        new ClassContentFrame with Frame {
          def clazz = clazz0
          def title =
            clazz0.kind + " " + clazz0.name + " in " + (clazz0.sym.owner.fullNameString('.'));
        }
      }
    }
    new RootFrame with Frame
  }
  override def longList(entity: ClassOrObject, category: Category)(implicit from: Frame) : NodeSeq = category match {
    case Classes | Objects => NodeSeq.Empty
    case _ => super.longList(entity, category)
  }

  trait Frame extends super.Frame {
    def longHeader(entity : Entity) = DefaultDocDriver.this.longHeader(entity)(this)
    def shortHeader(entity : Entity) = DefaultDocDriver.this.shortHeader(entity)(this)
  }

  import DocUtil._
  override def classBody(entity: ClassOrObject)(implicit from: Frame): NodeSeq =
    (subClasses.get(entity.sym) match {
    case Some(symbols) =>
      (<dl>
      <dt style="margin:10px 0 0 20px;"><b>Direct Known Subclasses:</b></dt>
      <dd>{symbols.mkXML("",", ","")(cls => {
        aref(urlFor(cls.sym), cls.path.map(_.name).mkString("",".",""));
      })}</dd>
      </dl><hr/>);
    case None =>
      NodeSeq.Empty
    })++super.classBody(entity);

  protected def urlFor(sym: Symbol)(implicit frame: Frame) = frame.urlFor(sym)

  override protected def decodeTag(tag: String): String = tag match {
    case "exception"  => "Throws"
    case "ex"         => "Examples"
    case "param"      => "Parameters"
    case "pre"        => "Precondition"
    case "return"     => "Returns"
    case "note"       => "Notes"
    case "see"        => "See Also"
    case tag => super.decodeTag(tag)
  }

  override protected def decodeOption(tag: String, option: String): NodeSeq = tag match {
    case "throws" if additions0.exceptions.contains(option) =>
      val (sym, s) = additions0.exceptions(option)
      val path = "../" //todo: fix path
      val href = path + sym.fullNameString('/') +
      (if (sym.isModule || sym.isModuleClass) NAME_SUFFIX_OBJECT else "") +
        "#" + s
      (<a href={href}>{option}</a>) ++ {Text(" - ")};
    case _ =>
      super.decodeOption(tag,option)
  }

  object roots extends jcl.LinkedHashMap[String,String];
  roots("classes") = "http://java.sun.com/j2se/1.5.0/docs/api";
  roots("rt") = roots("classes");
  private val SCALA_API_ROOT = "http://www.scala-lang.org/docu/files/api/";
  roots("scala-library") = SCALA_API_ROOT;

  private def keyFor(file: ZipFile): String = {
    var name = file.getName
    var idx = name.lastIndexOf(java.io.File.pathSeparator)
    if (idx == -1) idx = name.lastIndexOf('/')
    if (idx != -1) name = name.substring(idx + 1)
    if (name endsWith ".jar") name.substring(0, name.length - (".jar").length)
    else null
  }

  // <code>{Text(string + " - ")}</code>;
  override def hasLink0(sym: Symbol): Boolean = {
    if (sym == NoSymbol) return false;
    if (sym == AnyRefClass) {
      // AnyRefClass is a type alias, so the following logic
      // does not work.  AnyClass should have a link in
      // the same cases as AnyRefClass, so test it instead.
      return hasLink(AnyClass)
    }
    if (super.hasLink0(sym) && symbols.contains(sym))
      return true;
    if (SyntheticClasses contains sym)
      return true;
    if (sym.toplevelClass == NoSymbol) return false;
    val clazz = sym.toplevelClass.asInstanceOf[ClassSymbol];
    import scala.tools.nsc.io._;
    clazz.classFile match {
      case file : ZipArchive#FileEntry =>
        val key = keyFor(file.archive);
        if (key != null && roots.contains(key)) return true;
      case null =>
      case _ =>
    }
    false
  }

  def aref(href: String, label: String)(implicit frame: Frame) =
    frame.aref(href, "_self", label)

  protected def anchor(entity: Symbol)(implicit frame: Frame): NodeSeq =
    (<a name={Text(frame.docName(entity))}></a>)

  object symbols extends jcl.LinkedHashSet[Symbol]

  object allClasses extends jcl.LinkedHashMap[Package, jcl.LinkedHashSet[ClassOrObject]] {
    override def default(pkg: Package): jcl.LinkedHashSet[ClassOrObject] = {
      object ret extends jcl.LinkedHashSet[ClassOrObject]
      this(pkg) = ret
      ret
    }
  }

  object subClasses extends jcl.LinkedHashMap[Symbol, jcl.LinkedHashSet[ClassOrObject]] {
    override def default(key: Symbol) = {
      val ret = new jcl.LinkedHashSet[ClassOrObject]
      this(key) = ret
      ret
    }
  }

  override def rootFor(sym: Symbol): String = {
    assert(sym != NoSymbol)
    if (sym == definitions.AnyRefClass) {
      // AnyRefClass is a type alias, so the following logic
      // does not work.  AnyClass should have the same root,
      // so use it instead.
      return rootFor(definitions.AnyClass)
    }
    if (sym.toplevelClass == NoSymbol) return super.rootFor(sym)
    if (symbols.contains(sym.toplevelClass)) return super.rootFor(sym)
    if (SyntheticClasses contains sym)
      return SCALA_API_ROOT
    val clazz = sym.toplevelClass.asInstanceOf[ClassSymbol]
    import scala.tools.nsc.io._;
    clazz.classFile match {
      case file : ZipArchive#FileEntry =>
        val key = keyFor(file.archive)
        if (key != null && roots.contains(key)) {
          return roots(key) + '/'
        }
      case _ =>
    }
    super.rootFor(sym)
  }
}
