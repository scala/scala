//############################################################################
// Double Definitions
//############################################################################
// $Id$

//############################################################################
// Packages followed by objects/classes with same name

package    tic { class  Test1;   }
package    tic { object module1; }
package    tic { class  Test2;   }
// error: object tic would hide package tic
object     tic { def    fun = 0; }
package    tic { class  Test3;   }
package    tic { object module2; }
package    tic { class  Test4;   }
// error: tic.fun does not exist
object Testtic { tic.module1; tic.module2; tic.fun; }

package    tac { class Test1;    }
package    tac { object module1; }
package    tac { class Test2;    }
// error: case class tac (its factory method) would hide package tac
case class tac { def   fun = 0;  }
package    tac { class Test3;    }
package    tac { object module2; }
package    tac { class Test4;    }
// error: tac().fun does not exits
object Testtac { tac.module1; tac.module2; tac().fun; (new tac).fun; }

package    toc { class  Test1;   }
package    toc { object module1; }
package    toc { class  Test2;   }
class toc      { def    fun = 0; }
package    toc { class  Test3;   }
package    toc { object module2; }
package    toc { class  Test4;   }
object Testtoc { toc.module1; toc.module2; (new toc).fun; }

//############################################################################
// Objects/Classes followed by packages with same name

object     mic { def    fun = 0; }
// error: package mic would hide object mic
package    mic { object module;  }
// error: mic.module does not exist
object Testmic { mic.module; mic.fun; }

case class mac { def   fun = 0;  }
// error: package mac would hide case class mac (its factory method)
package    mac { object module;  }
// error: mac.module does not exist
object Testmac { mac.module; mac().fun; (new mac).fun; }

class moc      { def    fun = 0; }
package    moc { object module;  }
object Testmoc { moc.module; (new moc).fun; }

//############################################################################
// Objects followed by Objects/Classes with same name

object     bic { def fun1 = 0; }
// error: object bic redefines object bic
object     bic { def fun2 = 0; }
// error: bic.fun1 does not exist
object Testbic { bic.fun1; bic.fun2; }

object     bac { def fun1 = 0; }
// error: case class bac (its factory method) replaces object bac
case class bac { def fun2 = 0; }
// error: bac.fun1 does not exist
object Testbac { bac.fun1; bac().fun2; (new bac).fun2; }

object     boc { def fun1 = 0; }
class      boc { def fun2 = 0; }
object Testboc { boc.fun1; (new boc).fun2; }

//############################################################################
// Case classes followed by Objects/Classes with same name

case class cic { def fun1 = 0; }
// error: object cic replaces case class cic (its factory method)
object     cic { def fun2 = 0; }
// error: cic().fun1 does not exist
object Testcic { cic().fun1; (new cic).fun1; cic.fun2; }

case class cac { def fun1 = 0; }
// error: case class cac redefines case class cac
case class cac { def fun2 = 0; }
// error: cac().fun1 does not exist
// error: (new cac).fun1 does not exist
object Testcac { cac().fun1; (new cac).fun1; cac().fun2; (new cac).fun2; }

case class coc { def fun1 = 0; }
// error: class coc redefines case class coc
class      coc { def fun2 = 0; }
// error: (new coc).fun1 does not exist
object Testcoc { coc().fun1; (new coc).fun1; (new coc).fun2; }

//############################################################################
// Classes followed by Objects/Classes with same name

class      dic { def fun1 = 0; }
object     dic { def fun2 = 0; }
object Testdic { (new dic).fun1; dic.fun2; }

class      dac { def fun1 = 0; }
// error: case class dac redefines class dac
case class dac { def fun2 = 0; }
// error: (new dac).fun1 does not exist
object Testdac { (new dac).fun1; dac().fun2; (new dac).fun2; }

class      doc { def fun1 = 0; }
// error: class doc redefines class doc
class      doc { def fun2 = 0; }
// error: (new doc).fun1 does not exist
object Testdoc { (new doc).fun1; (new doc).fun2; }

//############################################################################
