/* NSC -- new Scala compiler
 * Copyright 2010-2013 LAMP/EPFL
 * @author  Stephane Micheloud
 */

package scala
package tools.nsc.doc.html

import scala.annotation.tailrec

/** Highlight the syntax of Scala code appearing in a `{{{` wiki block
  * (see method `HtmlPage.blockToHtml`).
  *
  * @author Stephane Micheloud
  * @version 1.0
  */
private[html] object SyntaxHigh {

  /** Reserved words, sorted alphabetically
    * (see [[scala.reflect.internal.StdNames]]) */
  val reserved = Array(
    "abstract", "case", "catch", "class", "def",
    "do", "else", "extends", "false", "final", "finally",
    "for", "if", "implicit", "import", "lazy", "match",
    "new", "null", "object", "override", "package",
    "private", "protected", "return", "sealed", "super",
    "this", "throw", "trait", "true", "try", "type",
    "val", "var", "while", "with", "yield").sorted

  /** Annotations, sorted alphabetically */
  val annotations = Array(
    "BeanProperty", "SerialVersionUID",
    "beanGetter", "beanSetter", "bridge",
    "deprecated", "deprecatedName", "deprecatedOverriding", "deprecatedInheritance",
    "elidable", "field", "getter", "inline",
    "migration", "native", "noinline", "param",
    "setter", "specialized", "strictfp", "switch",
    "tailrec", "throws", "transient",
    "unchecked", "uncheckedStable", "uncheckedVariance",
    "varargs", "volatile").sorted

  /** Standard library classes/objects, sorted alphabetically */
  val standards = Array(
    "Any", "AnyRef", "AnyVal", "App", "Array",
    "Boolean", "Byte", "Char", "Class", "ClassManifest", "ClassTag",
    "Console", "Double", "Enumeration", "Float", "Function", "Int",
    "List", "Long", "Manifest", "Map",
    "NoManifest", "None", "Nothing", "Null", "Object", "Option", "OptManifest",
    "Predef",
    "Seq", "Set", "Short", "Some", "String", "Symbol",
    "TypeTag", "Unit", "WeakTypeTag").sorted

}
