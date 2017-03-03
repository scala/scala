/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package doc

import java.io.File
import scala.language.postfixOps

/** An extended version of compiler settings, with additional Scaladoc-specific options.
  * @param error A function that prints a string to the appropriate error stream
  * @param printMsg A function that prints the string, without any extra boilerplate of error */
class Settings(error: String => Unit, val printMsg: String => Unit = println(_)) extends scala.tools.nsc.Settings(error) {

  // TODO 2.13 Remove
  private def removalIn213 = "This flag is scheduled for removal in 2.13. If you have a case where you need this flag then please report a bug."

  /** A setting that defines in which format the documentation is output. ''Note:'' this setting is currently always
    * `html`. */
  val docformat = ChoiceSetting (
    "-doc-format",
    "format",
    "Selects in which format documentation is rendered.",
    List("html"),
    "html"
  )

  /** A setting that defines the overall title of the documentation, typically the name of the library being
    * documented. */
  val doctitle = StringSetting (
    "-doc-title",
    "title",
    "The overall name of the Scaladoc site",
    ""
  )

  /** A setting that defines the overall version number of the documentation, typically the version of the library being
    * documented. */
  val docversion = StringSetting (
    "-doc-version",
    "version",
    "An optional version number, to be appended to the title",
    ""
  )

  val docfooter = StringSetting (
    "-doc-footer",
    "footer",
    "A footer on every Scaladoc page, by default the EPFL/Lightbend copyright notice. Can be overridden with a custom footer.",
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
    s"A URL pattern used to link to the source file; the following variables are available: €{TPL_NAME}, €{TPL_OWNER} and respectively €{FILE_PATH}. For example, for `scala.collection.Seq`, the variables will be expanded to `Seq`, `scala.collection` and respectively `scala/collection/Seq` (without the backquotes). To obtain a relative path for €{FILE_PATH} instead of an absolute one, use the ${sourcepath.name} setting.",
    ""
  )

  val docExternalDoc = MultiStringSetting (
    "-doc-external-doc",
    "external-doc",
    "comma-separated list of classpath_entry_path#doc_URL pairs describing external dependencies."
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

  val docImplicitsHide = MultiStringSetting (
	  "-implicits-hide",
    "implicit(s)",
    "Hide the members inherited by the given comma separated, fully qualified implicit conversions. Add dot (.) to include default conversions."
  )

  val docAuthor = BooleanSetting (
    "-author",
    "Include authors."
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

  /** The maximum number of normal classes to show in the diagram */
  val docDiagramsMaxNormalClasses = IntSetting(
    "-diagrams-max-classes",
    "The maximum number of superclasses or subclasses to show in a diagram",
    15,
    None,
    _ => None
  )

  /** The maximum number of implicit classes to show in the diagram */
  val docDiagramsMaxImplicitClasses = IntSetting(
    "-diagrams-max-implicits",
    "The maximum number of implicitly converted classes to show in a diagram",
    10,
    None,
    _ => None
  )

  val docDiagramsDotTimeout = IntSetting(
    "-diagrams-dot-timeout",
    "The timeout before the graphviz dot util is forcefully closed, in seconds (default: 10)",
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

  val docNoPrefixes = BooleanSetting (
    "-no-prefixes",
    "Prevents generating prefixes in types, possibly creating ambiguous references, but significantly speeding up scaladoc."
  )

  val docNoLinkWarnings = BooleanSetting (
    "-no-link-warnings",
    "Avoid warnings for ambiguous and incorrect links."
  )

  val docSkipPackages = StringSetting (
    "-skip-packages",
    "<package1>:...:<packageN>",
    "A colon-delimited list of fully qualified package names that will be skipped from scaladoc.",
    ""
  )

  // TODO 2.13 Remove
  val docExpandAllTypes = BooleanSetting (
    "-expand-all-types",
    "Expand all type aliases and abstract types into full template pages. (locally this can be done with the @template annotation)"
  ) withDeprecationMessage(removalIn213)

  val docGroups = BooleanSetting (
    "-groups",
    "Group similar functions together (based on the @group annotation)"
  )

  val docNoJavaComments = BooleanSetting (
    "-no-java-comments",
    "Prevents parsing and inclusion of comments from java sources."
  )

  // For improved help output.
  def scaladocSpecific = Set[Settings#Setting](
    docformat, doctitle, docfooter, docversion, docUncompilable, docsourceurl, docgenerator, docRootContent, useStupidTypes,
    docExternalDoc,
    docAuthor, docDiagrams, docDiagramsDebug, docDiagramsDotPath,
    docDiagramsDotTimeout, docDiagramsDotRestart,
    docImplicits, docImplicitsDebug, docImplicitsShowAll, docImplicitsHide, docImplicitsSoundShadowing,
    docDiagramsMaxNormalClasses, docDiagramsMaxImplicitClasses,
    docNoPrefixes, docNoLinkWarnings, docRawOutput, docSkipPackages,
    docExpandAllTypes, docGroups, docNoJavaComments
  )
  val isScaladocSpecific: String => Boolean = scaladocSpecific map (_.name)

  override def isScaladoc = true

  // set by the testsuite, when checking test output
  var scaladocQuietRun = false

  lazy val skipPackageNames =
    if (docSkipPackages.value == "")
      Set[String]()
    else
      docSkipPackages.value.toLowerCase.split(':').toSet

  def skipPackage(qname: String) =
    skipPackageNames(qname.toLowerCase)

  lazy val hiddenImplicits: Set[String] = {
    if (docImplicitsHide.value.isEmpty) hardcoded.commonConversionTargets
    else docImplicitsHide.value.toSet flatMap { name: String =>
      if(name == ".") hardcoded.commonConversionTargets
      else Set(name)
    }
  }

  def appendIndex(url: String): String = url.stripSuffix("index.html").stripSuffix("/") + "/index.html"

  lazy val extUrlMapping: Map[String, String] = docExternalDoc.value flatMap { s =>
    val idx = s.indexOf("#")
    if (idx > 0) {
      val (first, last) = s.splitAt(idx)
      Some(new File(first).getCanonicalPath -> appendIndex(last.substring(1)))
    } else {
      error(s"Illegal -doc-external-doc option; expected a pair with '#' separator, found: '$s'")
      None
    }
  } toMap

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
      ("scala.math.Numeric"                     -> ((tparam: String) => tparam + " is a numeric class, such as Int, Long, Float or Double")) +
      ("scala.math.Integral"                    -> ((tparam: String) => tparam + " is an integral numeric class, such as Int or Long")) +
      ("scala.math.Fractional"                  -> ((tparam: String) => tparam + " is a fractional numeric class, such as Float or Double")) +
      ("scala.reflect.Manifest"                 -> ((tparam: String) => tparam + " is accompanied by a Manifest, which is a runtime representation of its type that survives erasure")) +
      ("scala.reflect.ClassManifest"            -> ((tparam: String) => tparam + " is accompanied by a ClassManifest, which is a runtime representation of its type that survives erasure")) +
      ("scala.reflect.OptManifest"              -> ((tparam: String) => tparam + " is accompanied by an OptManifest, which can be either a runtime representation of its type or the NoManifest, which means the runtime type is not available")) +
      ("scala.reflect.ClassTag"                 -> ((tparam: String) => tparam + " is accompanied by a ClassTag, which is a runtime representation of its type that survives erasure")) +
      ("scala.reflect.api.TypeTags.WeakTypeTag" -> ((tparam: String) => tparam + " is accompanied by a WeakTypeTag, which is a runtime representation of its type that survives erasure")) +
      ("scala.reflect.api.TypeTags.TypeTag"     -> ((tparam: String) => tparam + " is accompanied by a TypeTag, which is a runtime representation of its type that survives erasure"))

    private val excludedClassnamePatterns = Set(
      """^scala.Tuple.*""",
      """^scala.Product.*""",
      """^scala.Function.*""",
      """^scala.runtime.AbstractFunction.*"""
    ) map (_.r)

    private val notExcludedClasses = Set(
      "scala.Tuple1",
      "scala.Tuple2",
      "scala.Product",
      "scala.Product1",
      "scala.Product2",
      "scala.Function",
      "scala.Function1",
      "scala.Function2",
      "scala.runtime.AbstractFunction0",
      "scala.runtime.AbstractFunction1",
      "scala.runtime.AbstractFunction2"
    )

    /**
     * Set of classes to exclude from index and diagrams
     * TODO: Should be configurable
     */
    def isExcluded(qname: String) = {
      excludedClassnamePatterns.exists(_.findFirstMatchIn(qname).isDefined) && !notExcludedClasses(qname)
    }

    /** Common conversion targets that affect any class in Scala */
    val commonConversionTargets = Set(
      "scala.Predef.StringFormat",
      "scala.Predef.any2stringadd",
      "scala.Predef.ArrowAssoc",
      "scala.Predef.Ensuring",
      "scala.collection.TraversableOnce.alternateImplicit")

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
