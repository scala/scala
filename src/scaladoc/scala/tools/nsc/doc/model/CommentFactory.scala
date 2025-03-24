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
package doc
package model

import base.comment._

import scala.collection.mutable
import scala.reflect.internal.util.Position

/** The comment parser transforms raw comment strings into `Comment` objects.
  * Call `parse` to run the parser. Note that the parser is stateless and
  * should only be built once for a given Scaladoc run.
  *
  * @author Manohar Jonnalagedda
  * @author Gilles Dubochet */
trait CommentFactory extends base.CommentFactoryBase {
  thisFactory: ModelFactory with CommentFactory with MemberLookup =>

  val global: Global
  import global.{ Symbol, NoSymbol }

  protected val commentCache = mutable.HashMap.empty[(Symbol, DocTemplateImpl), Option[Comment]]

  def comment(sym: Symbol, linkTarget: DocTemplateImpl, inTpl: DocTemplateImpl): Option[Comment] =
    commentCache.getOrElseUpdate((sym, inTpl), {
      defineComment(sym, linkTarget, inTpl)
    })

  /** A comment is usually created by the parser, however for some special
    * cases we have to give some `inTpl` comments (parent class for example)
    * to the comment of the symbol.
    * This function manages some of those cases : Param accessor and Primary constructor */
  def defineComment(sym: Symbol, linkTarget: DocTemplateImpl, inTpl: DocTemplateImpl):Option[Comment] = {

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
        val c = parse(rawComment, global.rawDocComment(sym), global.docCommentPos(sym), linkTarget)
        Some(c)
      }
      else None
    }

  }

  protected def parse(comment: String, src: String, pos: Position, linkTarget: DocTemplateImpl): Comment = {
    val sym = if (linkTarget eq null) NoSymbol else linkTarget.sym
    parseAtSymbol(comment, src, pos, sym)
  }

  /** Parses a string containing wiki syntax into a `Comment` object.
    * Note that the string is assumed to be clean:
    *  - Removed Scaladoc start and end markers.
    *  - Removed start-of-line star and one whitespace afterwards (if present).
    *  - Removed all end-of-line whitespace.
    *  - Only `endOfLine` is used to mark line endings. */
  def parseWiki(string: String, pos: Position, inTpl: DocTemplateImpl): Body = {
    val sym = if (inTpl eq null) NoSymbol else inTpl.sym
    parseWikiAtSymbol(string,pos, sym)
  }
}
