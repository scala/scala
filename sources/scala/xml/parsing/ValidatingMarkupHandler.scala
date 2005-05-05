package scala.xml.parsing ;

import scala.xml.dtd._ ;

abstract class ValidatingMarkupHandler extends MarkupHandler {

  final override val isValidating = true;

  final override def elemDecl(name: String, cmstr: String): Unit =
    decls = ElemDecl( name,  ContentModel.parse(cmstr)) :: decls;

  final override def attListDecl(name: String, attList: List[AttrDecl]): Unit =
    decls = AttListDecl( name, attList) :: decls;

  final override def parameterEntityDecl(name: String, edef: EntityDef): Unit =
    decls = ParameterEntityDecl( name, edef) :: decls;

  final override def parsedEntityDecl(name: String, edef: EntityDef): Unit =
    decls = ParsedEntityDecl( name, edef) :: decls;

  final override def unparsedEntityDecl(name: String, extID: ExternalID, notat: String): Unit =
    decls = UnparsedEntityDecl( name, extID, notat) :: decls;

  final override def notationDecl(notat: String, extID: ExternalID): Unit =
    decls = NotationDecl( notat, extID) :: decls;

  final override def peReference(name: String): Unit =
    decls = PEReference( name ) :: decls;

}
