/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc;

class Settings(error: String => unit) {

  private var allsettings: List[Setting] = List();

  val debuginfo     = BooleanSetting("-g", "Generate debugging info");
  val nowarnings    = BooleanSetting("-nowarn", "Generate no warnings");
  val verbose       = BooleanSetting("-verbose", "Output messages about what the compiler is doing");
  val classpath     = StringSetting ("-classpath", "path", "Specify where to find user class files",
                                     System.getProperty("scala.class.path",
                                       System.getProperty("java.class.path", ".")));
  val sourcepath    = StringSetting ("-sourcepath", "path", "Specify where to find input source files",
                                     System.getProperty("scala.source.path",
                                       System.getProperty("java.source.path", ".")));
  val bootclasspath = StringSetting ("-bootclasspath", "path", "Override location of bootstrap class files",
                                     System.getProperty("sun.boot.class.path", ""));
  val extdirs       = StringSetting ("-extdirs", "dirs", "Override location of installed extensions",
                                     System.getProperty("java.ext.dirs", ""));
  val outdir        = StringSetting ("-d", "directory", "Specify where to place generated class files", "");
  val encoding      = StringSetting ("-encoding", "encoding", "Specify character encoding used by source files", "ISO-8859-1");
  val separate      = ChoiceSetting ("-separate", "Read symbol files for separate compilation", List("yes","no"), "default");
  val target        = ChoiceSetting ("-target", "Specify which backend to use",  List("jvm", "msil"), "jvm");
  val debug         = BooleanSetting("-debug", "Output debugging messages");
  val statistics    = BooleanSetting("-statistics", "Print compiler statistics");
  val explaintypes  = BooleanSetting("-explaintypes", "Explain type errors in more detail");
  //val interpret     = BooleanSetting("-interpret", "Run interpreter");
  val resident      = BooleanSetting("-resident", "Compiler stays resident, files to compile are read from standard input");
  //val tokenMetric   = BooleanSetting("-tokenMetric", "only count tokens");
  val uniqid        = BooleanSetting("-uniqid", "Print identifiers with unique names (debugging option)");
  val printtypes    = BooleanSetting("-printtypes", "Print tree types (debugging option)");
  val prompt        = BooleanSetting("-prompt", "Display a prompt after each error (debugging option)");
  val noimports     = BooleanSetting("-noimports", "Compile without any implicit imports");
  val nopredefs     = BooleanSetting("-nopredefs", "Compile without any implicit predefined values");
  val skip          = PhasesSetting ("-skip", "Skip");
  val check         = PhasesSetting ("-check", "Check the tree at start of");
  val print         = PhasesSetting ("-print", "Print out program after");
  val printer       = ChoiceSetting ("-printer", "Printer to use", List("text", "html"), "text");
  val printfile     = StringSetting ("-printfile", "file", "Specify file in which to print trees", "-");
  val graph         = PhasesSetting ("-graph", "Graph the program after");
  val browse        = PhasesSetting ("-browse", "Browse the abstract syntax tree after");
  val stop          = PhasesSetting ("-stop", "Stop after phase");
  val log           = PhasesSetting ("-log", "Log operations in");
  val version       = BooleanSetting("-version", "Print product version and exit");
  val help          = BooleanSetting("-help", "Print a synopsis of standard options");

  val Xshowcls      = StringSetting ("-Xshowcls", "class", "Show class info", "");
  val Xshowobj      = StringSetting ("-Xshowobj", "object", "Show object info", "");
  val Xshowicode    = BooleanSetting("-Xshowicode", "Print the generated ICode");
  val Xgadt         = BooleanSetting("-Xgadt", "enable gadt for classes");

  /** A list of all settings */
  def allSettings: List[Setting] = allsettings.reverse;

  /** A base class for settings of all types.
   *  Subclasses each define a `value' field of the appropriate type.
   */
  abstract class Setting(name: String, descr: String) {

    /** If first arg defines this setting, consume it as well as all following
     *  args needed to define the setting. If this can be done without
     *  error, set value field and return suffix of args else
     *  issue error message and return empty.
     *  If first arg does not define this setting return args unchanged.
     */
    def tryToSet(args: List[String]): List[String];

    /** The syntax defining this setting in a help string */
    def helpSyntax: String = name;

    /** A description of the purpose of this setting in a help string */
    def helpDescription = descr;

    // initialization
    allsettings = this :: allsettings;
  }

  /** A setting represented by a boolean flag (false, unless set) */
  case class BooleanSetting(name: String, descr: String)
  extends Setting(name, descr) {
    var value: boolean = false;

    def tryToSet(args: List[String]): List[String] = args match {
      case n :: rest if (n == name) => value = true; rest
      case _ => args
    }
  }

  /** A setting represented by a string, (`default' unless set) */
  case class StringSetting(name: String, arg: String, descr: String, default: String)
  extends Setting(name, descr) {
    var value: String = default;

    def tryToSet(args: List[String]): List[String] = args match {
      case n :: rest if (n == name) =>
        if (rest.isEmpty) {
    error("missing argument");
    List()
  } else {
    value = rest.head;
    rest.tail
  }
      case _ => args
    }

    override def helpSyntax = name + " <" + arg + ">";
  }

  /** A setting represented by a string in a given set of `choices',
   *  (`default' unless set)
   */
  case class ChoiceSetting(name: String, descr: String, choices: List[String], default: String)
  extends Setting(name, descr + choices.mkString(" (", ",", ")")) {
    var value: String = default;

    private def argument: String = name.substring(1);

    def tryToSet(args: List[String]): List[String] = args match {
      case n :: rest if (n startsWith (name + ":")) =>
        val choice = n.substring(name.length() + 1);
        if (!(choices contains choice)) {
          error(
            if (choice == "") "missing " + argument
            else "unknown " + argument + " '" + choice + "'");
    List()
  } else {
          value = choice;
          rest
  }
      case _ => args
    }

    override def helpSyntax = name + ":<" + argument + ">";
  }

  /** A setting represented by a list of strings which should be prefixes of
   *  phase names. This is not checked here, however.
   *  (the empty list, unless set)
   */
  case class PhasesSetting(name: String, descr: String)
  extends Setting(name, descr + " <phases> (see below)") {
    var value: List[String] = List();

    def tryToSet(args: List[String]): List[String] = args match {
      case n :: rest if (n startsWith (name + ":")) =>
        val phase = n.substring(name.length() + 1);
        if (phase == "") {
    error("missing phase");
    List()
  } else {
          value = value ::: List(phase);
          rest
  }
      case _ => args
    }

    override def helpSyntax = name + ":<phase>";

    def contains(phasename: String): boolean =
      value exists (str => phasename startsWith str)
  }
}
