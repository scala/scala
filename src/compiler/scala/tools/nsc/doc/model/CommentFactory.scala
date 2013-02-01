/* NSC -- new Scala compiler
 * Copyright 2007-2013 LAMP/EPFL
 * @author  Manohar Jonnalagedda
 */

package scala.tools.nsc
package doc
package model

import base.comment._

import reporters.Reporter
import scala.collection._
import scala.reflect.internal.util.{NoPosition, Position}
import scala.language.postfixOps

/** The comment parser transforms raw comment strings into `Comment` objects.
  * Call `parse` to run the parser. Note that the parser is stateless and
  * should only be built once for a given Scaladoc run.
  *
  * @param reporter The reporter on which user messages (error, warnings) should be printed.
  *
  * @author Manohar Jonnalagedda
  * @author Gilles Dubochet */
trait CommentFactory extends base.CommentFactoryBase {
  thisFactory: ModelFactory with CommentFactory with MemberLookup =>

  val global: Global
  import global.{ reporter, definitions, Symbol }

  protected val commentCache = mutable.HashMap.empty[(Symbol, TemplateImpl), Comment]

  def addCommentBody(sym: Symbol, inTpl: TemplateImpl, docStr: String, docPos: global.Position): Symbol = {
    commentCache += (sym, inTpl) -> parse(docStr, docStr, docPos, None)
    sym
  }

  def comment(sym: Symbol, currentTpl: Option[DocTemplateImpl], inTpl: DocTemplateImpl): Option[Comment] = {
    val key = (sym, inTpl)
    if (commentCache isDefinedAt key)
      Some(commentCache(key))
    else {
      val c = defineComment(sym, currentTpl, inTpl)
      if (c isDefined) commentCache += (sym, inTpl) -> c.get
      c
    }
  }

  /** A comment is usualy created by the parser, however for some special
    * cases we have to give some `inTpl` comments (parent class for example)
    * to the comment of the symbol.
    * This function manages some of those cases : Param accessor and Primary constructor */
  def defineComment(sym: Symbol, currentTpl: Option[DocTemplateImpl], inTpl: DocTemplateImpl):Option[Comment] = {

    //param accessor case
    // We just need the @param argument, we put it into the body
    if( sym.isParamAccessor &&
        inTpl.comment.isDefined &&
        inTpl.comment.get.valueParams.isDefinedAt(sym.encodedName)) {
      val comContent = Some(inTpl.comment.get.valueParams(sym.encodedName))
      Some(createComment(body0 = comContent))
    }

    // Primary constructor case
    // We need some content of the class definition : @constructor for the body,
    // @param and @deprecated, we can add some more if necessary
    else if (sym.isPrimaryConstructor && inTpl.comment.isDefined ) {
      val tplComment = inTpl.comment.get
      // If there is nothing to put into the comment there is no need to create it
      if(tplComment.constructor.isDefined ||
        tplComment.throws != Map.empty ||
        tplComment.valueParams != Map.empty ||
        tplComment.typeParams != Map.empty ||
        tplComment.deprecated.isDefined
        )
        Some(createComment( body0 = tplComment.constructor,
                            throws0 = tplComment.throws,
                            valueParams0 = tplComment.valueParams,
                            typeParams0 = tplComment.typeParams,
                            deprecated0 = tplComment.deprecated
                            ))
      else None
    }

    //other comment cases
    // parse function will make the comment
    else {
      val rawComment = global.expandedDocComment(sym, inTpl.sym).trim
      if (rawComment != "") {
        val tplOpt = if (currentTpl.isDefined) currentTpl else Some(inTpl)
        val c = parse(rawComment, global.rawDocComment(sym), global.docCommentPos(sym), tplOpt)
        Some(c)
      }
      else None
    }

  }

  protected def parse(comment: String, src: String, pos: Position, inTplOpt: Option[DocTemplateImpl] = None): Comment = {
    assert(!inTplOpt.isDefined || inTplOpt.get != null)
    parseAtSymbol(comment, src, pos, inTplOpt map (_.sym))
  }

  /** Parses a string containing wiki syntax into a `Comment` object.
    * Note that the string is assumed to be clean:
    *  - Removed Scaladoc start and end markers.
    *  - Removed start-of-line star and one whitespace afterwards (if present).
    *  - Removed all end-of-line whitespace.
    *  - Only `endOfLine` is used to mark line endings. */
  def parseWiki(string: String, pos: Position, inTplOpt: Option[DocTemplateImpl]): Body = {
    assert(!inTplOpt.isDefined || inTplOpt.get != null)
    parseWikiAtSymbol(string,pos, inTplOpt map (_.sym))
  }
}
