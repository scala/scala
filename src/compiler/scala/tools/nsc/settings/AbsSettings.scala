/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package settings

/** A Settings abstraction boiled out of the original highly mutable Settings
 *  class with the intention of creating an ImmutableSettings which can be used
 *  interchangeably.   Except of course without the mutants.
 */

trait AbsSettings extends scala.reflect.internal.settings.AbsSettings {
  type Setting <: AbsSetting      // Fix to the concrete Setting type
  type ResultOfTryToSet           // List[String] in mutable, (Settings, List[String]) in immutable
  def errorFn: String => Unit
  protected def allSettings: scala.collection.Set[Setting]

  // settings minus internal usage settings
  def visibleSettings = allSettings filterNot (_.isInternalOnly)

  // only settings which differ from default
  def userSetSettings = visibleSettings filterNot (_.isDefault)

  // an argument list which (should) be usable to recreate the Settings
  def recreateArgs = userSetSettings.toList flatMap (_.unparse)

  // checks both name and any available abbreviations
  def lookupSetting(cmd: String): Option[Setting] = allSettings find (_ respondsTo cmd)

  // two AbsSettings objects are equal if their visible settings are equal.
  override def hashCode() = visibleSettings.size  // going for cheap
  override def equals(that: Any) = that match {
    case s: AbsSettings => this.userSetSettings == s.userSetSettings
    case _              => false
  }
  override def toString() = {
    val uss    = userSetSettings
    val indent = if (uss.nonEmpty) " " * 2 else ""
    uss.mkString(f"Settings {%n$indent", f"%n$indent", f"%n}%n")
  }
  def toConciseString = userSetSettings.mkString("(", " ", ")")

  def checkDependencies =
    visibleSettings filterNot (_.isDefault) forall (setting => setting.dependencies forall {
      case (dep, value) =>
        (Option(dep.value) exists (_.toString == value)) || {
          errorFn("incomplete option %s (requires %s)".format(setting.name, dep.name))
          false
        }
    })

  trait AbsSetting extends Ordered[Setting] with AbsSettingValue {
    def name: String
    def helpDescription: String
    def unparse: List[String]     // A list of Strings which can recreate this setting.

    /* For tools which need to populate lists of available choices */
    def choices : List[String] = Nil

    /** In mutable Settings, these return the same object with a var set.
     *  In immutable, of course they will return a new object, which means
     *  we can't use "this.type", at least not in a non-casty manner, which
     *  is unfortunate because we lose type information without it.
     *
     *  ...but now they're this.type because of #3462.  The immutable
     *  side doesn't exist yet anyway.
     */
    def withAbbreviation(name: String): this.type
    def withHelpSyntax(help: String): this.type
    def withDeprecationMessage(msg: String): this.type

    def helpSyntax: String = name
    def deprecationMessage: Option[String] = None
    def abbreviations: List[String] = Nil
    def dependencies: List[(Setting, String)] = Nil
    def respondsTo(label: String) = (name == label) || (abbreviations contains label)

    /** If the setting should not appear in help output, etc. */
    private var internalSetting = false
    def isInternalOnly = internalSetting
    def internalOnly(): this.type = {
      internalSetting = true
      this
    }

    /** Issue error and return */
    def errorAndValue[T](msg: String, x: T): T = { errorFn(msg) ; x }

    /** If this method returns true, print the [[help]] message and exit. */
    def isHelping: Boolean = false

    /** The help message to be printed if [[isHelping]]. */
    def help: String = ""

    /** After correct Setting has been selected, tryToSet is called with the
     *  remainder of the command line.  It consumes any applicable arguments and
     *  returns the unconsumed ones.
     */
    protected[nsc] def tryToSet(args: List[String]): Option[ResultOfTryToSet]

    /** Commands which can take lists of arguments in form -Xfoo:bar,baz override
     *  this method and accept them as a list.  It returns List[String] for
     *  consistency with tryToSet, and should return its incoming arguments
     *  unmodified on failure, and Nil on success.
     */
    protected[nsc] def tryToSetColon(args: List[String]): Option[ResultOfTryToSet] =
      errorAndValue("'%s' does not accept multiple arguments" format name, None)

    /** Attempt to set from a properties file style property value.
     *  Currently used by Eclipse SDT only.
     *  !!! Needs test.
     */
    def tryToSetFromPropertyValue(s: String): Unit = tryToSet(s :: Nil)

    /** These categorizations are so the help output shows -X and -P among
     *  the standard options and -Y among the advanced options.
     */
    def isAdvanced   = name match { case "-Y" => true ; case "-X" => false ; case _  => name startsWith "-X" }
    def isPrivate    = name match { case "-Y" => false ; case _  => name startsWith "-Y" }
    def isStandard   = !isAdvanced && !isPrivate
    def isForDebug   = name endsWith "-debug" // by convention, i.e. -Ytyper-debug
    def isDeprecated = deprecationMessage.isDefined

    def compare(that: Setting): Int = name compare that.name

    /** Equality tries to sidestep all the drama and define it simply and
     *  in one place: two AbsSetting objects are equal if their names and
     *  values compare equal.
     */
    override def equals(that: Any) = that match {
      case x: AbsSettings#AbsSetting  => (name == x.name) && (value == x.value)
      case _                          => false
    }
    override def hashCode() = name.hashCode + value.hashCode
    override def toString() = name + " = " + (if (value == "") "\"\"" else value)
  }

  trait InternalSetting extends AbsSetting {
    override def isInternalOnly = true
  }
}
