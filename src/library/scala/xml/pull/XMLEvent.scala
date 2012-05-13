/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.xml
package pull

/** An XML event for pull parsing.  All events received during
 * parsing will be one of the subclasses of this trait.
 */
trait XMLEvent

/**
 * An Element's start tag was encountered.
 * @param pre prefix, if any, on the element.  This is the `xs` in `<xs:string>foo</xs:string>`.
 * @param label the name of the element, not including the prefix
 * @param attrs any attributes on the element
 */
case class EvElemStart(pre: String, label: String, attrs: MetaData, scope: NamespaceBinding) extends XMLEvent

/**
 * An Element's end tag was encountered.
 * @param pre prefix, if any, on the element.  This is the `xs` in `<xs:string>foo</xs:string>`.
 * @param label the name of the element, not including the prefix
 */
case class EvElemEnd(pre: String, label: String) extends XMLEvent

/**
 * A text node was encountered.
 * @param text the text that was found
 */
case class EvText(text: String) extends XMLEvent

/** An entity reference was encountered.
 * @param entity the name of the entity, e.g. `gt` when encountering the entity `&gt;`
 */
case class EvEntityRef(entity: String) extends XMLEvent

/**
 * A processing instruction was encountered.
 * @param target the "PITarget" of the processing instruction.  For the instruction `<?foo bar="baz"?>`, the target would
 * be `foo`
 * @param text the remainder of the instruction.  For the instruction `<?foo bar="baz"?>`, the text would
 * be `bar="baz"`
 * @see [[http://www.w3.org/TR/REC-xml/#sec-pi]]
 */
case class EvProcInstr(target: String, text: String) extends XMLEvent

/**
 * A comment was encountered
 * @param text the text of the comment
 */
case class EvComment(text: String) extends XMLEvent
