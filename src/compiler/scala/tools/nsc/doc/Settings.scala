/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package doc

import java.io.File
import java.lang.System
import language.postfixOps

/** An extended version of compiler settings, with additional Scaladoc-specific options.
  * @param error A function that prints a string to the appropriate error stream
  * @param print A function that prints the string, without any extra boilerplate of error */
class Settings(error: String => Unit, val printMsg: String => Unit = println(_)) extends scala.tools.nsc.Settings(error) {

  /** A setting that defines in which format the documentation is output. ''Note:'' this setting is currently always
    * `html`. */
  val docformat = ChoiceSetting (
    "-doc-format",
    "format",
    "Selects in which format documentation is rendered",
    List("html"),
    "html"
  )

  /** A setting that defines the overall title of the documentation, typically the name of the library being
    * documented. ''Note:'' This setting is currently not used. */
  val doctitle = StringSetting (
    "-doc-title",
    "title",
    "The overall name of the Scaladoc site",
    ""
  )

  /** A setting that defines the overall version number of the documentation, typically the version of the library being
    * documented. ''Note:'' This setting is currently not used. */
  val docversion = StringSetting (
    "-doc-version",
    "version",
    "An optional version number, to be appended to the title",
    ""
  )

  val docfooter = StringSetting (
    "-doc-footer",
    "footer",
    "A footer on every ScalaDoc page, by default the EPFL/Typesafe copyright notice. Can be overridden with a custom footer.",
    ""
  )

  val docUncompilable = StringSetting (
    "-doc-no-compile",
    "path",
    "A directory containing sources which should be parsed, no more (e.g. AnyRef.scala)",
    ""
  )

  lazy val uncompilableFiles = docUncompilable.value match {
    case ""     => Nil
    case path   => io.Directory(path).deepFiles filter (_ hasExtension "scala") toList
  }

  /** A setting that defines a URL to be concatenated with source locations and show a link to source files.
   * If needed the sourcepath option can be used to exclude undesired initial part of the link to sources */
  val docsourceurl = StringSetting (
    "-doc-source-url",
    "url",
    "A URL pattern used to build links to template sources; use variables, for example: ?{TPL_NAME} ('Seq'), ?{TPL_OWNER} ('scala.collection'), ?{FILE_PATH} ('scala/collection/Seq')",
    ""
  )

  val useStupidTypes = BooleanSetting (
    "-Yuse-stupid-types",
    "Print the types of inherited members as seen from their original definition context. Hint: you don't want to do that!"
  )

  val docgenerator = StringSetting (
    "-doc-generator",
    "class-name",
    "The fully qualified name of a doclet class, which will be used to generate the documentation",
    "scala.tools.nsc.doc.html.Doclet"
  )

  val docRootContent = PathSetting (
    "-doc-root-content",
    "The file from which the root package documentation should be imported.",
    ""
  )

  val docImplicits = BooleanSetting (
    "-implicits",
    "Document members inherited by implicit conversions."
  )

  val docImplicitsDebug = BooleanSetting (
    "-implicits-debug",
    "Show debugging information for members inherited by implicit conversions."
  )

  val docImplicitsShowAll = BooleanSetting (
    "-implicits-show-all",
    "Show members inherited by implicit conversions that are impossible in the default scope. " +
    "(for example conversions that require Numeric[String] to be in scope)"
  )

  val docImplicitsSoundShadowing = BooleanSetting (
    "-implicits-sound-shadowing",
    "Use a sound implicit shadowing calculation. Note: this interacts badly with usecases, so " +
    "only use it if you haven't defined usecase for implicitly inherited members."
  )

  val docDiagrams = BooleanSetting (
    "-diagrams",
    "Create inheritance diagrams for classes, traits and packages."
  )

  val docDiagramsDebug = BooleanSetting (
    "-diagrams-debug",
    "Show debugging information for the diagram creation process."
  )

  val docDiagramsDotPath = PathSetting (
    "-diagrams-dot-path",
    "The path to the dot executable used to generate the inheritance diagrams. Eg: /usr/bin/dot",
    "dot" // by default, just pick up the system-wide dot
  )

  /** The maxium nuber of normal classes to show in the diagram */
  val docDiagramsMaxNormalClasses = IntSetting(
    "-diagrams-max-classes",
    "The maximum number of superclasses or subclasses to show in a diagram",
    15,
    None,
    _ => None
  )

  /** The maxium nuber of implcit classes to show in the diagram */
  val docDiagramsMaxImplicitClasses = IntSetting(
    "-diagrams-max-implicits",
    "The maximum number of implicitly converted classes to show in a diagram",
    10,
    None,
    _ => None
  )

  val docDiagramsDotTimeout = IntSetting(
    "-diagrams-dot-timeout",
    "The timeout before the graphviz dot util is forecefully closed, in seconds (default: 10)",
    10,
    None,
    _ => None
  )

  val docDiagramsDotRestart = IntSetting(
    "-diagrams-dot-restart",
    "The number of times to restart a malfunctioning dot process before disabling diagrams (default: 5)",
    5,
    None,
    _ => None
  )

  val docRawOutput = BooleanSetting (
    "-raw-output",
    "For each html file, create another .html.raw file containing only the text. (can be used for quickly diffing two scaladoc outputs)"
  )

  // Somewhere slightly before r18708 scaladoc stopped building unless the
  // self-type check was suppressed.  I hijacked the slotted-for-removal-anyway
  // suppress-vt-warnings option and renamed it for this purpose.
  noSelfCheck.value = true

  // For improved help output.
  def scaladocSpecific = Set[Settings#Setting](
    docformat, doctitle, docfooter, docversion, docUncompilable, docsourceurl, docgenerator, docRootContent, useStupidTypes,
    docDiagrams, docDiagramsDebug, docDiagramsDotPath,
    docDiagramsDotTimeout, docDiagramsDotRestart,
    docImplicits, docImplicitsDebug, docImplicitsShowAll,
    docDiagramsMaxNormalClasses, docDiagramsMaxImplicitClasses
  )
  val isScaladocSpecific: String => Boolean = scaladocSpecific map (_.name)

  override def isScaladoc = true

  // set by the testsuite, when checking test output
  var scaladocQuietRun = false

  /**
   *  This is the hardcoded area of Scaladoc. This is where "undesirable" stuff gets eliminated. I know it's not pretty,
   *  but ultimately scaladoc has to be useful. :)
   */
  object hardcoded {

    /** The common context bounds and some humanly explanations. Feel free to add more explanations
     *  `<root>.scala.package.Numeric` is the type class
     *  `tparam` is the name of the type parameter it gets (this only describes type classes with 1 type param)
     *  the function result should be a humanly-understandable description of the type class
     */
    val knownTypeClasses: Map[String, String => String] = Map() +
      // TODO: Bring up to date and test these
      ("scala.package.Numeric"       -> ((tparam: String) => tparam + " is a numeric class, such as Int, Long, Float or Double")) +
      ("scala.package.Integral"      -> ((tparam: String) => tparam + " is an integral numeric class, such as Int or Long")) +
      ("scala.package.Fractional"    -> ((tparam: String) => tparam + " is a fractional numeric class, such as Float or Double")) +
      ("scala.reflect.Manifest"      -> ((tparam: String) => tparam + " is accompanied by a Manifest, which is a runtime representation of its type that survives erasure")) +
      ("scala.reflect.ClassManifest" -> ((tparam: String) => tparam + " is accompanied by a ClassManifest, which is a runtime representation of its type that survives erasure")) +
      ("scala.reflect.OptManifest"   -> ((tparam: String) => tparam + " is accompanied by an OptManifest, which can be either a runtime representation of its type or the NoManifest, which means the runtime type is not available")) +
      ("scala.reflect.ClassTag"      -> ((tparam: String) => tparam + " is accompanied by a ClassTag, which is a runtime representation of its type that survives erasure")) +
      ("scala.reflect.AbsTypeTag"    -> ((tparam: String) => tparam + " is accompanied by an AbsTypeTag, which is a runtime representation of its type that survives erasure")) +
      ("scala.reflect.TypeTag"       -> ((tparam: String) => tparam + " is accompanied by a TypeTag, which is a runtime representation of its type that survives erasure"))

    /**
     * Set of classes to exclude from index and diagrams
     * TODO: Should be configurable
     */
    def isExcluded(qname: String) = {
      ( ( qname.startsWith("scala.Tuple") || qname.startsWith("scala.Product") ||
         qname.startsWith("scala.Function") || qname.startsWith("scala.runtime.AbstractFunction")
       ) && !(
        qname == "scala.Tuple1" || qname == "scala.Tuple2" ||
        qname == "scala.Product" || qname == "scala.Product1" || qname == "scala.Product2" ||
        qname == "scala.Function" || qname == "scala.Function1" || qname == "scala.Function2" ||
        qname == "scala.runtime.AbstractFunction0" || qname == "scala.runtime.AbstractFunction1" ||
        qname == "scala.runtime.AbstractFunction2"
      )
     )
    }

    /** Common conversion targets that affect any class in Scala */
    val commonConversionTargets = List(
      "scala.Predef.any2stringfmt",
      "scala.Predef.any2stringadd",
      "scala.Predef.any2ArrowAssoc",
      "scala.Predef.any2Ensuring",
      "scala.collection.TraversableOnce.alternateImplicit")

    /** There's a reason all these are specialized by hand but documenting each of them is beyond the point */
    val arraySkipConversions = List(
      "scala.Predef.refArrayOps",
      "scala.Predef.intArrayOps",
      "scala.Predef.doubleArrayOps",
      "scala.Predef.longArrayOps",
      "scala.Predef.floatArrayOps",
      "scala.Predef.charArrayOps",
      "scala.Predef.byteArrayOps",
      "scala.Predef.shortArrayOps",
      "scala.Predef.booleanArrayOps",
      "scala.Predef.unitArrayOps",
      "scala.LowPriorityImplicits.wrapRefArray",
      "scala.LowPriorityImplicits.wrapIntArray",
      "scala.LowPriorityImplicits.wrapDoubleArray",
      "scala.LowPriorityImplicits.wrapLongArray",
      "scala.LowPriorityImplicits.wrapFloatArray",
      "scala.LowPriorityImplicits.wrapCharArray",
      "scala.LowPriorityImplicits.wrapByteArray",
      "scala.LowPriorityImplicits.wrapShortArray",
      "scala.LowPriorityImplicits.wrapBooleanArray",
      "scala.LowPriorityImplicits.wrapUnitArray",
      "scala.LowPriorityImplicits.genericWrapArray")

    // included as names as here we don't have access to a Global with Definitions :(
    def valueClassList = List("unit", "boolean", "byte", "short", "char", "int", "long", "float", "double")
    def valueClassFilterPrefixes = List("scala.LowPriorityImplicits", "scala.Predef")

    /** Dirty, dirty, dirty hack: the value params conversions can all kick in -- and they are disambiguated by priority
     *  but showing priority in scaladoc would make no sense -- so we have to manually remove the conversions that we
     *  know will never get a chance to kick in. Anyway, DIRTY DIRTY DIRTY! */
    def valueClassFilter(value: String, conversionName: String): Boolean = {
      val valueName = value.toLowerCase
      val otherValues = valueClassList.filterNot(_ == valueName)

      for (prefix <- valueClassFilterPrefixes)
        if (conversionName.startsWith(prefix))
          for (otherValue <- otherValues)
            if (conversionName.startsWith(prefix + "." + otherValue))
              return false

      true
    }
  }
}
