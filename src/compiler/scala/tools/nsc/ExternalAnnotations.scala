/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc

import java.net.URLClassLoader

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Codec
import scala.reflect.io.AbstractFile
import scala.tools.nsc.Reporting.WarningCategory
import scala.tools.nsc.settings.DefaultPathFactory
import scala.tools.nsc.util.ClassPath

/**
 * TODO
 *   - checking of external annotations: annotations exist, selected symbols exist
 *   - respect target meta-annotations
 *   - overloading resolution
 *   - support named arguments, reorder according to params
 *   - multiple selections, like imports (a.b.{c, d})
 */
trait ExternalAnnotations { self: Global =>

  case class ExtAnn(ownerNames: List[Name], annot: String, args: List[Any]) {
    final def matchesOwner(owner: Symbol): Boolean = {
      @tailrec def checkOwners(owner: Symbol, names: List[Name]): Boolean = names match {
        case n :: ns if !owner.isRoot =>
          val on = if (owner.isModuleClass) owner.name.toTermName else owner.name
          on == n && checkOwners(owner.owner, ns)
        case ns =>
          ns.isEmpty && owner.isRoot
      }

      if (owner.isPackageClass) false // don't add annotations to packages, sub-packages
      else if (ownerNames.head == termNames.WILDCARD) checkOwners(owner.owner, ownerNames.tail)
      else checkOwners(owner, ownerNames)
    }

    def toAnnotationInfo: Option[AnnotationInfo] = {
      def toTreeArg(a: Any): Tree = Literal(Constant(a))

      def toConstArg(a: Any): ClassfileAnnotArg = a match {
        case a: Array[_] => ArrayAnnotArg(a.map(toConstArg))
        case c           => LiteralAnnotArg(Constant(c))
      }

      val annotCls = rootMirror.getClassIfDefined(annot)
      if (annotCls == NoSymbol) {
        runReporting.warning(NoPosition, s"annotation class '$annot' for external annotation not found", WarningCategory.OtherExternalAnnotations, "")
        None
      }
      else {
        val isConst = annotCls.isNonBottomSubClass(definitions.ConstantAnnotationClass)
        def annotArgs = args.iterator.map(a => toTreeArg(a)).toList
        def annotAssocs = args.zip(annotCls.primaryConstructor.info.params).iterator.map({
          case (arg, paramSym) => (paramSym.name, toConstArg(arg))
        }).toList
        Some(AnnotationInfo(annotCls.tpe, if (isConst) Nil else annotArgs, if (isConst) annotAssocs else Nil))
      }
    }
  }

  def parseExternalAnnotations(s: String, filename: String): List[ExtAnn] =
    parseExternalAnnotations(newCompilationUnit(s, filename))

  def parseExternalAnnotations(unit: CompilationUnit): List[ExtAnn] = {
    import syntaxAnalyzer._

    // Override error reporting, external annotations only generate warnings
    object parser extends UnitParser(unit) {
      val errPos = mutable.Set.empty[Position]

      override def syntaxError(offset: Offset, msg: String): Unit = {
        val pos = o2p(offset)
        if (!errPos(pos)) {
          warning(pos, msg)
          errPos += pos
        }
      }

      // Ignore warning category, always use OtherExternalAnnotations
      override def warning(offset: Offset, msg: String, category: WarningCategory): Unit = warning(offset, msg)

      def warning(offset: Offset, msg: String): Unit = warning(o2p(offset), msg)

      def warning(pos: Position, msg: String): Unit =
        if (!errPos(pos.focus))
          runReporting.warning(pos, msg, WarningCategory.OtherExternalAnnotations, "")
    }

    import parser._

    import scala.tools.nsc.ast.parser.Tokens._

    def warnUnexpectedToken(): Unit =
      parser.warning(in.offset, s"invalid syntax in external annotation, unexpected ${token2string(in.token)}")

    def nameByCase(n: Name) = {
      val first = n.charAt(0)
      if (first >= 'A' && first <= 'Z') n.toTypeName
      else n.toTermName
    }

    def id(): Option[Name] = in.token match {
      case BACKQUOTED_IDENT =>
        val n = ident()
        Some {
          if (in.token == HASH) { in.nextToken(); n.toTypeName }
          else if (in.token == IDENTIFIER && in.name.toString == "$") { in.nextToken(); n.toTermName }
          else nameByCase(n)
        }

      case IDENTIFIER =>
        val n = ident()
        Some {
          if (in.token == HASH) { in.nextToken(); n.toTypeName }
          else if (n.endsWith('$')) n.dropRight(1).toTermName
          else nameByCase(n)
        }

      case USCORE =>
        in.nextToken()
        Some(termNames.WILDCARD)

      case _ =>
        warnUnexpectedToken()
        None
    }

    def ownerNames(): List[Name] = {
      var res = List.empty[Name]
      id().foreach(res ::= _)
      while (in.token == DOT) {
        in.nextToken()
        id().foreach(res ::= _)
      }
      res
    }

    def annotAndArgs(ann: Tree): Option[(String, List[Any])] = {
      def annotPath(t: Tree, names: List[Name] = Nil): Option[String] = t match {
        case Select(qual, name) => annotPath(qual, name :: names)
        case Ident(name) => Some((name :: names).mkString("."))
        case t =>
          parser.warning(t.pos, s"unexpected tree in annotation selector: $t")
          None
      }

      def annotArg(tree: Tree): Option[Any] = tree match {
        case Literal(Constant(v)) => Some(v)
        case NamedArg(_, Literal(Constant(v))) =>
          parser.warning(tree.pos, "named arguments not supported, treating as positional")
          Some(v)
        case t =>
          parser.warning(t.pos, s"annotation arguments need to be constants, found $t")
          None
      }

      ann match {
        case Apply(Select(New(tp), _), args) =>
          annotPath(tp).flatMap(p => {
            val aso = args.map(annotArg)
            if (aso.exists(_.isEmpty)) None else Some((p, aso.map(_.get)))
          })
        case t =>
          parser.warning(t.pos, s"unexpected annotation tree, found $t")
          None
      }
    }

    val res = mutable.ListBuffer.empty[ExtAnn]
    while (isAnnotation) {
      val annots = parser.annotations(skipNewLines = true).flatMap(annotAndArgs)
      if (!isIdent)
        parser.warning(in.offset, s"stale external annotation, no annotation targets found")
      while (isIdent) {
        val owners = ownerNames()
        if (owners.nonEmpty) for ((ann, args) <- annots)
          res += ExtAnn(owners, ann, args)
        newLinesOpt()
      }
    }
    if (in.token != EOF)
      warnUnexpectedToken()
    res.toList
  }

  private var extAnnsByName: Map[String, List[ExtAnn]] = null
  private var extAnnsByOwnerName: Map[String, List[ExtAnn]] = null

  private def extAnnotFiles: List[AbstractFile] = {
    val ps = ClassPath.expandPath(settings.YexternalAnnotationFiles.value, expandStar = false)
    ps.map(DefaultPathFactory.getFile)
  }

  private def extAnnotsFromClasspath: List[(String, String)] = {
    if (settings.YexternalAnnotations.value) {
      import scala.jdk.CollectionConverters._
      val loader = new URLClassLoader(classPath.asURLs.toArray)
      val urls = loader.getResources("external-annotations.txt").asScala.toList
      val contents = urls.map(url => (scala.io.Source.fromInputStream(url.openStream())(Codec.UTF8).mkString, url.getPath))
      loader.close()
      contents
    } else Nil

  }

  private def initExternalAnnotations() = if (extAnnsByName == null) {
    val parsedExtAnns =
      extAnnotFiles.flatMap(f => parseExternalAnnotations(new CompilationUnit(getSourceFile(f)))) ++
        extAnnotsFromClasspath.flatMap({ case (content, filename) => parseExternalAnnotations(content, filename)})
    val (wild, name) = parsedExtAnns.partition(_.ownerNames.head == termNames.WILDCARD)
    extAnnsByName = name.groupBy(_.ownerNames.head.toString)
    extAnnsByOwnerName = wild.groupBy(_.ownerNames.tail.head.toString)
  }

  // some symbols are completed multiple times (lazy type -> lazy type -> actual type, unpickler); add annots only once
  private lazy val extAnnsAdded: mutable.Set[Symbol] = {
    initExternalAnnotations()
    perRunCaches.newSet()
  }

  def addExternalAnnotations(sym: Symbol): Unit = if (!extAnnsAdded(sym)) {
    val matchingName = extAnnsByName.getOrElse(sym.name.toString, Nil) ++ extAnnsByOwnerName.getOrElse(sym.owner.name.toString, Nil)
    val annInfos = matchingName.filter(_.matchesOwner(sym)).flatMap(_.toAnnotationInfo)
    if (annInfos.nonEmpty) {
      val syms =
        if (sym.isModuleClass) List(sym, sym.module)
        else if (sym.isModule) List(sym, sym.moduleClass)
        else List(sym)
      for (sym <- syms) {
        extAnnsAdded += sym
        annInfos.foreach(sym.addAnnotation)
      }
    }
  }
}
