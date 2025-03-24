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

package scala.tools.nsc
package settings

/** A Settings abstraction boiled out of the original highly mutable Settings
 *  class with the intention of creating an ImmutableSettings which can be used
 *  interchangeably.   Except of course without the mutants.
 */

trait AbsSettings extends scala.reflect.internal.settings.AbsSettings {
  type Setting <: AbsSetting      // Fix to the concrete Setting type
  type ResultOfTryToSet           // List[String] in MutableSettings
  def errorFn: String => Unit
  protected def allSettings: scala.collection.Map[String, Setting]

  // settings minus internal usage settings
  def visibleSettings: List[Setting] = allSettings.valuesIterator.filterNot(_.isInternalOnly).toList

  // only settings which differ from default
  def userSetSettings: List[Setting] = visibleSettings.filterNot(_.isDefault)

  // an argument list which (should) be usable to recreate the Settings
  def recreateArgs: List[String] = userSetSettings flatMap (_.unparse)

  // checks both name and any available abbreviations
  def lookupSetting(cmd: String): Option[Setting] = allSettings.valuesIterator find (_ respondsTo cmd)

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

    /** Issue error and return the value. */
    def errorAndValue[A](msg: String, x: A): A = { errorFn(msg) ; x }

    /** If this method returns true, print the [[help]] message and exit. */
    def isHelping: Boolean = false

    /** The help message to be printed if [[isHelping]]. */
    def help: String = ""

    /** Setting is presented the remaining command line arguments.
     *  It should consume any applicable args and return the rest,
     *  or `None` on error.
     */
    protected[nsc] def tryToSet(args: List[String]): Option[ResultOfTryToSet]

    /** Setting is presented arguments in form -Xfoo:bar,baz.
     *  It should consume all the arguments and return an empty list,
     *  or `None` on error. Unconsumed args may error.
     */
    protected[nsc] def tryToSetColon(args: List[String]): Option[ResultOfTryToSet]

    /** Attempt to set from a properties file style property value.
     *  Currently used by Eclipse SDT only.
     *  !!! Needs test.
     */
    def tryToSetFromPropertyValue(s: String): Unit = tryToSet(s :: Nil)

    /** Standard options are shown on the `-help` output,
     *  advanced on `-X`, private on `-Y`, warning on `-W`, verbose on `-V`.
     *
     *  The single char options themselves, including `-P`, are explained on `-help`.
     *  Additionally, `-Werror` is on `-help` and `-Xlint` on `-W`.
     */
    def isAdvanced   = name.startsWith("-X") && name != "-X"
    def isPrivate    = name.startsWith("-Y") && name != "-Y"
    def isVerbose    = name.startsWith("-V") && name != "-V"
    def isWarning    = name.startsWith("-W") && name != "-W" || name == "-Xlint"
    def isStandard   = !isAdvanced && !isPrivate && !isWarning && !isVerbose || name == "-Werror"
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
