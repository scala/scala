/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

object GenerateFunctionConverters {
  case class Artifact(name: String, content: String)

  val copyright =
    s"""/*
       | * Scala (https://www.scala-lang.org)
       | *
       | * Copyright EPFL and Lightbend, Inc. dba Akka
       | *
       | * Licensed under Apache License 2.0
       | * (http://www.apache.org/licenses/LICENSE-2.0).
       | *
       | * See the NOTICE file distributed with this work for
       | * additional information regarding copyright ownership.
       | */
       |
       |// GENERATED CODE: DO NOT EDIT.
       |""".stripMargin

  val packaging = "package scala.jdk"

  import scala.tools.nsc._
  val settings = new Settings(msg => sys.error(msg))
  def us(cl: ClassLoader): List[String] = cl match {
    case ucl: java.net.URLClassLoader => ucl.getURLs.map(u => new java.io.File(u.toURI).getAbsolutePath).toList ::: us(ucl.getParent)
    case _ => Nil
  }
  settings.classpath.value = us(settings.getClass.getClassLoader).mkString(java.io.File.pathSeparator)
  val compiler = new Global(settings)
  val run = new compiler.Run

  import compiler._, definitions._
  locally {
    // make sure `java.lang.Double` prints as `java.lang.Double`, not just `Double` (which resolves to `scala.Double`)
    val f = classOf[scala.reflect.internal.Definitions#DefinitionsClass].getDeclaredField("UnqualifiedOwners")
    f.setAccessible(true)
    f.set(definitions, definitions.UnqualifiedOwners.filter(_.fullNameString != "java.lang"))
  }

  def primitiveBox(tp: Type): Type = tp.typeSymbol match {
    case UnitClass    => BoxedUnitClass.tpe
    case ByteClass    => BoxedByteClass.tpe
    case ShortClass   => BoxedShortClass.tpe
    case CharClass    => BoxedCharacterClass.tpe
    case IntClass     => BoxedIntClass.tpe
    case LongClass    => BoxedLongClass.tpe
    case FloatClass   => BoxedFloatClass.tpe
    case DoubleClass  => BoxedDoubleClass.tpe
    case BooleanClass => BoxedBooleanClass.tpe
    case _            => tp
  }

  implicit class IndentMe(v: Vector[String]) {
    def indent: Vector[String] = v.map("  " + _)
  }

  implicit class FlattenMe(v: Vector[Vector[String]]) {
    def mkVec(join: String = ""): Vector[String] = {
      val vb = Vector.newBuilder[String]
      var first = true
      v.foreach{ vi =>
        if (!first) vb += join
        first = false
        vb ++= vi
      }
      vb.result()
    }
  }

  implicit class DoubleFlattenMe(v: Vector[Vector[Vector[String]]]) {
    def mkVecVec(join: String = ""): Vector[String] = {
      val vb = Vector.newBuilder[String]
      var first = true
      v.foreach{ vi =>
        if (!first) { vb += join; vb += join }
        first = false
        var ifirst = true
        vi.foreach{ vj =>
          if (!ifirst) vb += join
          ifirst = false
          vb ++= vj
        }
      }
      vb.result()
    }
  }

  implicit class SplitMyLinesAndStuff(s: String) {
    // work around scala/bug#11125
    def toVec = Predef.augmentString(s).lines.toVector
    def nonBlank = s.trim.length > 0
  }

  implicit class TreeToText(t: Tree) {
    // work around scala/bug#11125
    def text = Predef.augmentString(showCode(t).replace("$", "")).lines.toVector
  }

  case class Prioritized(lines: Vector[String], priority: Int) {
    def withPriority(i: Int) = copy(priority = i)
  }

  case class SamConversionCode(
      base: String,
      wrappedAsScala: Vector[String],
      asScalaAnyVal: Vector[String],
      implicitToScala: Vector[String],
      asScalaDef: Vector[String],
      wrappedAsJava: Vector[String],
      asJavaAnyVal: Vector[String],
      implicitToJava: Prioritized,
      asJavaDef: Vector[String]
  ) {
    def impls: Vector[Vector[String]] = Vector(wrappedAsScala, asScalaAnyVal, wrappedAsJava, asJavaAnyVal)
    def defs: Vector[Vector[String]] = Vector(asScalaDef, asJavaDef)
    def withPriority(i: Int): SamConversionCode = copy(implicitToJava = implicitToJava.withPriority(i))
  }
  object SamConversionCode {
    def apply(scc: SamConversionCode*): (Vector[String], Vector[String], Vector[Vector[String]]) = {
      val sccDepthSet = scc.map(_.implicitToJava.priority).toSet
      val codes =
        {
          if (sccDepthSet != (0 to sccDepthSet.max).toSet) {
            val sccDepthMap = sccDepthSet.toList.sorted.zipWithIndex.toMap
            scc.map(x => x.withPriority(sccDepthMap(x.implicitToJava.priority)))
          }
          else scc
        }.toVector.sortBy(_.base)
      def priorityName(n: Int, pure: Boolean = false): String = {
        val pre =
          if (pure) s"Priority${n}FunctionExtensions"
          else s"trait ${priorityName(n, pure = true)}"
        if (!pure && n < (sccDepthSet.size-1)) s"$pre extends ${priorityName(n+1, pure = true)}" else pre
      }
      val impls =
        "object FunctionWrappers {" +: {
          codes.map(_.impls).mkVecVec().indent
        } :+ "}"
      val explicitDefs = codes.map(_.defs).mkVecVec()
      val traits = codes.groupBy(_.implicitToJava.priority).toVector.sortBy(- _._1).map{ case (k,vs) =>
        s"import language.implicitConversions" +:
        "" +:
        s"${priorityName(k)} {" +:
          s"  import FunctionWrappers._" +:
          s"  " +:
          {
            vs.map(_.implicitToJava.lines).mkVec().indent ++
              (
                if (k == 0) Vector.fill(3)("  ") ++ codes.map(_.implicitToScala).mkVec().indent
                else Vector()
              )
          } :+
          s"}"
      }
      (impls, explicitDefs, traits)
    }
  }

  private def buildWrappersViaReflection: Seq[SamConversionCode] = {

    val pack: Symbol = rootMirror.getPackageIfDefined("java.util.function")

    case class Jfn(iface: Symbol, sam: Symbol) {
      lazy val genericCount = iface.typeParams.length
      lazy val name = sam.name.toTermName
      lazy val title = iface.name.encoded
      lazy val params = sam.info.params
      lazy val sig = sam typeSignatureIn iface.info
      lazy val pTypes = sig.params.map(_.info)
      lazy val rType = sig.resultType
      def arity = params.length
    }

    val sams = pack.info.decls.
      map(d => (d, d.typeSignature.members.filter(_.isAbstract).toList)).
      collect{ case (d, m :: Nil) if d.isAbstract => Jfn(d, m) }

    def generate(jfn: Jfn): SamConversionCode = {
      def mkRef(tp: Type): Tree = if (tp.typeSymbol.isTypeParameter) Ident(tp.typeSymbol.name.toTypeName) else tq"$tp"

      // Types for the Java SAM and the corresponding Scala function, plus all type parameters
      val scalaType = gen.mkAttributedRef(FunctionClass(jfn.arity))
      val javaType = gen.mkAttributedRef(jfn.iface)
      val tnParams: List[TypeName] = jfn.iface.typeParams.map(_.name.toTypeName)
      val tdParams: List[TypeDef] = tnParams.map(TypeDef(NoMods, _, Nil, EmptyTree))
      val javaTargs: List[Tree] = tdParams.map(_.name).map(Ident(_))
      val scalaTargTps = jfn.pTypes :+ jfn.rType
      val scalaTargBoxedTps = scalaTargTps.map(primitiveBox)
      val scalaTargs: List[Tree] = scalaTargTps.map(mkRef)
      val scalaTargsBoxed: List[Tree] = scalaTargBoxedTps.map(mkRef)
      val boxComment =
        if (scalaTargTps.map(_.typeSymbol) != scalaTargBoxedTps.map(_.typeSymbol))
          Literal(Constant("primitiveComment"))
        else
          Literal(Constant("noComment"))

      // Conversion wrappers have three or four components that we need to name
      // (1) The wrapper class that wraps a Java SAM as Scala function, or vice versa (ClassN)
      // (2) A value class that provides .asJava or .asScala to request the conversion (ValCN)
      // (3) A name for an explicit conversion method (DefN)
      // (4) An implicit conversion method name (ImpN) that invokes the value class

      // Names for Java conversions to Scala
      val j2sClassN = TypeName("FromJava" + jfn.title)
      val j2sCompanionN = j2sClassN.toTermName
      val j2sValCN = TypeName("Rich" + jfn.title + "As" + scalaType.name.encoded)
      val j2sDefN = TermName("asScalaFrom" + jfn.title)
      val j2sImpN = TermName("enrichAsScalaFrom" + jfn.title)

      // Names for Scala conversions to Java
      val s2jAsJavaTitle = TermName("asJava" + jfn.title)
      val s2jClassN = TypeName("AsJava" + jfn.title)
      val s2jCompanionN = s2jClassN.toTermName
      val s2jValCN = TypeName("Rich" + scalaType.name.encoded + "As" + jfn.title)
      val s2jDefN = TermName("asJava" + jfn.title)
      val s2jImpN = TermName("enrichAsJava" + jfn.title)

      // Argument lists for the function / SAM
      val vParams = (jfn.params zip jfn.pTypes).map{ case (p,t) =>
        ValDef(NoMods, p.name.toTermName, if (t.typeSymbol.isTypeParameter) Ident(t.typeSymbol.name) else gen.mkAttributedRef(t.typeSymbol), EmptyTree)
      }
      val vParamRefs = vParams.map(_.name).map(Ident(_))

      val j2sClassTree =
        q"""case class $j2sClassN[..$tdParams](jf: $javaType[..$javaTargs]) extends $scalaType[..$scalaTargs] {
          def apply(..$vParams) = jf.${jfn.name}(..$vParamRefs)
        }"""

      val j2sValCTree =
        q"""class $j2sValCN[..$tdParams](private val underlying: $javaType[..$javaTargs]) extends AnyVal {
          @inline def asScala: $scalaType[..$scalaTargs] = underlying match {
            case $s2jCompanionN(sf) => sf.asInstanceOf[$scalaType[..$scalaTargs]]
            case _ => new $j2sClassN[..$tnParams](underlying)
          }
        }"""

      val j2sDefTree =
        q"""@deprecated($boxComment) @inline def $j2sDefN[..$tdParams](jf: $javaType[..$javaTargs]): $scalaType[..$scalaTargsBoxed] = jf match {
          case $s2jCompanionN(f) => f.asInstanceOf[$scalaType[..$scalaTargsBoxed]]
          case _ => new $j2sClassN[..$tnParams](jf).asInstanceOf[$scalaType[..$scalaTargsBoxed]]
        }"""

      val j2sImpTree =
        q"""@inline implicit def $j2sImpN[..$tdParams](jf: $javaType[..$javaTargs]): $j2sValCN[..$tnParams] = new $j2sValCN[..$tnParams](jf)"""

      val s2jClassTree =
        q"""case class $s2jClassN[..$tdParams](sf: $scalaType[..$scalaTargs]) extends $javaType[..$javaTargs] {
          def ${jfn.name}(..$vParams) = sf.apply(..$vParamRefs)
        }"""

      val s2jDefTree =
        q"""@deprecated($boxComment) @inline def $s2jDefN[..$tdParams](sf: $scalaType[..$scalaTargsBoxed]): $javaType[..$javaTargs] = (sf: AnyRef) match {
          case $j2sCompanionN(f) => f.asInstanceOf[$javaType[..$javaTargs]]
          case _ => new $s2jClassN[..$tnParams](sf.asInstanceOf[$scalaType[..$scalaTargs]])
        }"""

      // This is especially tricky because functions are contravariant in their arguments
      // Need to prevent e.g. Any => String from "downcasting" itself to Int => String; we want the more exact conversion
      val (s2jImpTree, priority) =
      if (jfn.pTypes.forall(! _.isFinalType) && jfn.sig == jfn.sam.typeSignature)
        (
          q"""@inline implicit def $s2jImpN[..$tdParams](sf: $scalaType[..$scalaTargs]): $s2jValCN[..$tnParams] = new $s2jValCN[..$tnParams](sf)""",
          tdParams.length
        )
      else {
        // Some types are not generic or are re-used; we had better catch those.
        // Making up new type names, so switch everything to TypeName or TypeDef
        // Instead of foo[A](f: (Int, A) => Long): Fuu[A] = new Foo[A](f)
        //    we want foo[X, A](f: (X, A) => Long)(implicit evX: Int =:= X): Fuu[A] = new Foo[A](f.asInstanceOf[(Int, A) => Long])
        // Instead of bar[A](f: A => A): Brr[A] = new Foo[A](f)
        //    we want bar[A, B](f: A => B)(implicit evB: A =:= B): Brr[A] = new Foo[A](f.asInstanceOf[A => B])
        val An = "A(\\d+)".r
        val numberedA = collection.mutable.Set.empty[Int]
        val evidences = collection.mutable.ArrayBuffer.empty[(TypeName, TypeName)]
        numberedA ++= scalaTargs.map(_.toString).collect{ case An(digits) if (digits.length < 10) => digits.toInt }
        val scalafnTnames = (jfn.pTypes :+ jfn.rType).zipWithIndex.map{
          case (pt, i) if (i < jfn.pTypes.length && pt.isFinalType) || (!pt.isFinalType && jfn.pTypes.take(i).exists(_ == pt)) =>
            val j = Iterator.from(i).dropWhile(numberedA).next
            val genericName = TypeName(s"A$j")
            numberedA += j
            evidences += ((genericName, pt.typeSymbol.name.toTypeName))
            genericName
          case (pt, _) => pt.typeSymbol.name.toTypeName
        }
        val scalafnTdefs = scalafnTnames.
          map(TypeDef(NoMods, _, Nil, EmptyTree)).
          dropRight(if (jfn.rType.isFinalType) 1 else 0)
        val evs = evidences.map{ case (generic, specific) => ValDef(NoMods, TermName("ev"+generic.toString), tq"$generic =:= $specific", EmptyTree) }
        val tree =
          q"""@inline implicit def $s2jImpN[..$scalafnTdefs](sf: $scalaType[..$scalafnTnames])(implicit ..$evs): $s2jValCN[..$tnParams] =
              new $s2jValCN[..$tnParams](sf.asInstanceOf[$scalaType[..$scalaTargs]])
            """
        (tree, tdParams.length)
      }

      val s2jValFullNameAsJavaMethodTree =
        if (priority > 0)
          q"""@inline def $s2jAsJavaTitle: $javaType[..$javaTargs] = underlying match {
            case $j2sCompanionN(sf) => sf.asInstanceOf[$javaType[..$javaTargs]]
            case _ => new $s2jClassN[..$tnParams](underlying)
          }"""
        else EmptyTree

      val s2jValCTree =
        q"""class $s2jValCN[..$tdParams](private val underlying: $scalaType[..$scalaTargs]) extends AnyVal {
          @inline def asJava: $javaType[..$javaTargs] = underlying match {
            case $j2sCompanionN(jf) => jf.asInstanceOf[$javaType[..$javaTargs]]
            case _ => new $s2jClassN[..$tnParams](underlying)
          }
          $s2jValFullNameAsJavaMethodTree
        }"""

      SamConversionCode(
        base = jfn.title,
        wrappedAsScala = j2sClassTree.text,
        asScalaAnyVal = j2sValCTree.text,
        implicitToScala = j2sImpTree.text,
        asScalaDef = j2sDefTree.text,
        wrappedAsJava = s2jClassTree.text,
        asJavaAnyVal = s2jValCTree.text,
        implicitToJava = Prioritized(s2jImpTree.text, priority),
        asJavaDef = s2jDefTree.text
      )
    }

    sams.toSeq.map(generate)
  }

  def sourceFile(subPack: String, body: String): String =
    s"""$copyright
       |
       |$packaging$subPack
       |
       |$body
       |""".stripMargin

  def sameText(f: java.io.File, text: String): Boolean = {
    val x = scala.io.Source.fromFile(f)
    val lines = try { x.getLines().toVector } finally { x.close }
    // work around scala/bug#11125
    lines.iterator.filter(_.nonBlank) == Predef.augmentString(text).lines.filter(_.nonBlank)
  }

  def write(outDir: java.io.File, artifact: Artifact): Unit = {
    val f = scala.tools.nsc.io.Path(outDir.getAbsolutePath) / artifact.name
    if (!f.exists || !sameText(f.jfile, artifact.content)) {
      f.parent.createDirectory(force = true)
      f.toFile writeAll artifact.content
    }
  }

  def run(outDir: java.io.File): Unit = {
    val (impls, explicitDefs, defss) = SamConversionCode(buildWrappersViaReflection: _*)
    val javaFunConvsNoComments =
      s"""/** This object contains methods that convert between Scala and Java function types.
         |  *
         |  * The explicit conversion methods defined here are intended to be used in Java code. For Scala
         |  * code, it is recommended to use the extension methods defined in [[scala.jdk.FunctionConverters]].
         |  *
         |  * For details how the function converters work, see [[scala.jdk.FunctionConverters]].
         |  *
         |  */
         |object FunctionConverters {
         |  import scala.jdk.FunctionWrappers._
         |
         |${explicitDefs.indent.mkString("\n")}
         |}""".stripMargin

    // cannot generate comments with quasiquotes
    val javaFunConvs = javaFunConvsNoComments.replace("""  @deprecated("primitiveComment") """,
      s"""  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
         |    * primitive type `scala.X` to improve compatibility when using it in Java code (the
         |    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
         |    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
         |    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
         |    */
         |""".stripMargin + "  ").replace("""@deprecated("noComment") """, "")

    val scalaFunConvs =
      """/** This object provides extension methods that convert between Scala and Java function types.
        |  *
        |  * When writing Java code, use the explicit conversion methods defined in
        |  * [[javaapi.FunctionConverters]] instead.
        |  *
        |  * Using the `.asJava` extension method on a Scala function produces the most specific possible
        |  * Java function type:
        |  *
        |  * {{{
        |  *   scala> import scala.jdk.FunctionConverters._
        |  *   scala> val f = (x: Int) => x + 1
        |  *
        |  *   scala> val jf1 = f.asJava
        |  *   jf1: java.util.function.IntUnaryOperator = ...
        |  * }}}
        |  *
        |  * More generic Java function types can be created using the corresponding `asJavaXYZ` extension
        |  * method:
        |  *
        |  * {{{
        |  *   scala> val jf2 = f.asJavaFunction
        |  *   jf2: java.util.function.Function[Int,Int] = ...
        |  *
        |  *   scala> val jf3 = f.asJavaUnaryOperator
        |  *   jf3: java.util.function.UnaryOperator[Int] = ...
        |  * }}}
        |  *
        |  * Converting a Java function to Scala is done using the `asScala` extension method:
        |  *
        |  * {{{
        |  *   scala> List(1,2,3).map(jf2.asScala)
        |  *   res1: List[Int] = List(2, 3, 4)
        |  * }}}
        |  */
        |object FunctionConverters extends Priority0FunctionExtensions""".stripMargin

    write(outDir, Artifact("jdk/javaapi/FunctionConverters.scala", sourceFile(".javaapi", javaFunConvs)))
    write(outDir, Artifact("jdk/FunctionConverters.scala", sourceFile("", scalaFunConvs)))
    write(outDir, Artifact("jdk/FunctionWrappers.scala", sourceFile("", impls.mkString("\n"))))
    write(outDir, Artifact("jdk/FunctionExtensions.scala", sourceFile("", defss.map(_.mkString("\n")).mkString("\n\n\n\n"))))
  }
}
