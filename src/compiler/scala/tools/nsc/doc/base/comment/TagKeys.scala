/* NSC -- new Scala compiler
 * Copyright 2007-2013 LAMP/EPFL
 * @author Eugene Vigdorchik
 */

package scala.tools.nsc
package doc
package base
package comment

object TagKeys extends Enumeration {
  val authorKey = Value("author")
  val seeKey = Value("see")
  val returnKey = Value("return")
  val throwsKey = Value("throws")
  val paramKey = Value("param")
  val tparamKey = Value("tparam")
  val versionKey = Value("version")
  val sinceKey = Value("since")
  val todoKey = Value("todo")
  val deprecatedKey = Value("deprecated")
  val noteKey = Value("note")
  val exampleKey = Value("example")
  val constructorKey = Value("constructor")
  val groupKey = Value("group")
  val groupdescKey = Value("groupdesc")
  val groupnameKey = Value("groupname")
  val groupprioKey = Value("groupprio")
  val inheritDiagramKey = Value("inheritanceDiagram")
  val contentDiagramKey = Value("contentDiagram")

  def docKeys: Set[String] = values map valueToString

  implicit def valueToString(v: Value) = v.toString
}
