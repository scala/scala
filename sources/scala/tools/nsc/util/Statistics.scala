/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */

// $Id$

package scala.tools.nsc.util;

abstract class Statistics {

  val global: Global;
  import global._;

  def print(phase: Phase) = {
    inform("*** Cumulative statistics at phase " + phase);
    inform("#tree nodes  : " + nodeCount);
    inform("#identifiers : " + analyzer.idcnt);
    inform("#selections  : " + analyzer.selcnt);
    inform("#applications: " + analyzer.appcnt);
    inform("#implicits   : " + analyzer.implcnt);
    inform("#typecreates : " + accesses);
    inform("#uniquetypes : " + uniques);
    inform("#collisions  : " + collisions);
    inform("#symbols     : " + symbolCount);
    inform("#type symbols: " + typeSymbolCount);
    inform("#class symbols: " + classSymbolCount);
    inform("#singleton closures: " + singletonClosureCount);
    inform("#compound closures : " + compoundClosureCount);
    inform("#typeref closures  : " + typerefClosureCount);
  }

}

