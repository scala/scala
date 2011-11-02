import sbt._

/**
 * @param root the root of an svn repository
 * @author Moix Grégory
 */
class SVN(root: Path) {
  /** Location of tool which parses svn revision in git-svn repository. */
  val GitSvnRevTool = root / "tools" / "get-scala-revision"
  val GitSvnRegex   = """^Revision:\s*(\d+).*""".r

  /**
   * Gets the revision number of the repository given through the constructor of the class
   * It assumes that svn or git is installed on the running computer. Return 0 if it was not
   * able to found the revision number
   */
  def getRevisionNumber: Int = getSvn orElse getGit getOrElse 0  
  def getSvn: Option[Int] = {
    /** Doing this the hard way trying to suppress the svn error message
     *  on stderr.  Could not figure out how to do it simply in sbt.
     */
    val pb = new java.lang.ProcessBuilder("svn", "info")
    pb directory root.asFile
    pb redirectErrorStream true
    
    Process(pb).lines_! foreach {
      case GitSvnRegex(rev) => return Some(rev.toInt)
      case _                => ()
    }
    None
  }
   
  def getGit: Option[Int] =    
    try   { Some(Process(GitSvnRevTool.toString, root).!!.trim.toInt) }
    catch { case _: Exception => None }
}
