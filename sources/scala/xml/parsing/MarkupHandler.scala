package scala.xml.parsing;

import scala.collection.immutable.Map ;
import scala.collection.mutable ;

/** @todo: make ConstructingMarkupHandler */
abstract class MarkupHandler[MarkupType, AVType] {

  def element(pos: int, label: String, attrMap1: mutable.Map[String,AttribValue[AVType]], args: mutable.Buffer[MarkupType]): MarkupType;

  def CharData(pos: Int, charData: scala.xml.CharData ): MarkupType;
  def ProcInstr(pos: Int, procInstr: scala.xml.ProcInstr ): MarkupType;
  def Comment(pos: Int, comment: scala.xml.Comment ): MarkupType;
  def EntityRef(pos: Int, n: String): MarkupType;

  def Text(pos: Int, mode: Boolean, txt:String): MarkupType;

}
