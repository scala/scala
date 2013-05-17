/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package xml
package factory

/** This class logs what the nodefactory is actually doing.
 *  If you want to see what happens during loading, use it like this:
{{{
object testLogged extends App {
  val x = new scala.xml.parsing.NoBindingFactoryAdapter
              with scala.xml.factory.LoggedNodeFactory[scala.xml.Elem] {
            override def log(s: String) = println(s)
          }

  Console.println("Start")
  val doc = x.load(new java.net.URL("http://example.com/file.xml"))
  Console.println("End")
  Console.println(doc)
}
}}}
 *
 *  @author  Burak Emir
 *  @version 1.0
 */
@deprecated("This trait will be removed.", "2.11")
trait LoggedNodeFactory[A <: Node] extends NodeFactory[A] {
  // configuration values
  val logNode      = true
  val logText      = false
  val logComment   = false
  val logProcInstr = false

  final val NONE  = 0
  final val CACHE = 1
  final val FULL  = 2
  /** 0 = no logging, 1 = cache hits, 2 = detail */
  val logCompressLevel  = 1

  // methods of NodeFactory

  /** logged version of makeNode method */
  override def makeNode(pre: String, label: String, attrSeq: MetaData,
                        scope: NamespaceBinding, children: Seq[Node]): A = {
    if (logNode)
      log("[makeNode for "+label+"]")

    val hash = Utility.hashCode(pre, label, attrSeq.##, scope.##, children)

    /*
    if(logCompressLevel >= FULL) {
      log("[hashcode total:"+hash);
      log(" elem name "+uname+" hash "+ ? ));
      log(" attrs     "+attrSeq+" hash "+attrSeq.hashCode());
      log(" children :"+children+" hash "+children.hashCode());
    }
    */
    if (!cache.get( hash ).isEmpty && (logCompressLevel >= CACHE))
      log("[cache hit !]")

    super.makeNode(pre, label, attrSeq, scope, children)
  }

  override def makeText(s: String) = {
    if (logText)
      log("[makeText:\""+s+"\"]")
    super.makeText(s)
  }

  override def makeComment(s: String): Seq[Comment] = {
    if (logComment)
      log("[makeComment:\""+s+"\"]")
    super.makeComment(s)
  }

  override def makeProcInstr(t: String, s: String): Seq[ProcInstr] = {
    if (logProcInstr)
      log("[makeProcInstr:\""+t+" "+ s+"\"]")
    super.makeProcInstr(t, s)
  }

  @deprecated("This method and its usages will be removed. Use a debugger to debug code.", "2.11")
  def log(msg: String): Unit = {}
}
