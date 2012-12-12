package scala.tools.nsc
package doc
package model

import base._

/** This trait extracts all required information for documentation from compilation units */
trait MemberLookup extends base.MemberLookupBase {
  thisFactory: ModelFactory =>

  import global._

  override def internalLink(sym: Symbol, site: Symbol): Option[LinkTo] =
    findTemplateMaybe(sym) match {
      case Some(tpl) => Some(LinkToTpl(tpl))
      case None =>
        findTemplateMaybe(site) flatMap { inTpl =>
          inTpl.members find (_.asInstanceOf[EntityImpl].sym == sym) map (LinkToMember(_, inTpl))
        }
    }

  override def chooseLink(links: List[LinkTo]): LinkTo = {
    val mbrs = links.collect {
      case lm@LinkToMember(mbr: MemberEntity, _) => (mbr, lm)
    }
    if (mbrs.isEmpty)
      links.head
    else
      mbrs.min(Ordering[MemberEntity].on[(MemberEntity, LinkTo)](_._1))._2
  }
}
