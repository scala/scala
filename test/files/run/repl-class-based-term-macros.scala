import scala.tools.nsc.Settings
import scala.tools.partest.ReplTest

object Test extends ReplTest {
  override def transformSettings(s: Settings) = {
    s.Yreplclassbased.value = true
    s
  }

  def code = """
  import scala.reflect.macros.blackbox.Context
import language.experimental.macros
import scala.reflect
import scala.reflect.macros.{ blackbox, whitebox }

def implBBC(c: blackbox.Context)                                = c.universe.reify(())
def implWBC(c: whitebox.Context)                                = c.universe.reify(())
def implRBBC(c: reflect.macros.blackbox.Context)                = c.universe.reify(())
def implRWBC(c: reflect.macros.whitebox.Context)                = c.universe.reify(())
def implSRBBC(c: scala.reflect.macros.blackbox.Context)         = c.universe.reify(())
def implSRWBC(c: scala.reflect.macros.whitebox.Context)         = c.universe.reify(())
def implRSRBBC(c: _root_.scala.reflect.macros.blackbox.Context) = c.universe.reify(())
def implRSRWBC(c: _root_.scala.reflect.macros.whitebox.Context) = c.universe.reify(())

def fooBBC: Unit    = macro implBBC
def fooWBC: Unit    = macro implWBC
def fooRBBC: Unit   = macro implRBBC
def fooRWBC: Unit   = macro implRWBC
def fooSRBBC: Unit  = macro implSRBBC
def fooSRWBC: Unit  = macro implSRWBC
def fooRSRBBC: Unit = macro implRSRBBC
def fooRSRWBC: Unit = macro implRSRWBC

// fooBBC
// fooWBC
// fooRBBC
// fooRWBC
// fooSRBBC
// fooSRWBC
// fooRSRBBC
// fooRSRWBC


object MacrosModule {
  def implBBC(c: blackbox.Context)                                = c.universe.reify(())
  def implWBC(c: whitebox.Context)                                = c.universe.reify(())
  def implRBBC(c: reflect.macros.blackbox.Context)                = c.universe.reify(())
  def implRWBC(c: reflect.macros.whitebox.Context)                = c.universe.reify(())
  def implSRBBC(c: scala.reflect.macros.blackbox.Context)         = c.universe.reify(())
  def implSRWBC(c: scala.reflect.macros.whitebox.Context)         = c.universe.reify(())
  def implRSRBBC(c: _root_.scala.reflect.macros.blackbox.Context) = c.universe.reify(())
  def implRSRWBC(c: _root_.scala.reflect.macros.whitebox.Context) = c.universe.reify(())
}

def barBBC: Unit    = macro MacrosModule.implBBC
def barWBC: Unit    = macro MacrosModule.implWBC
def barRBBC: Unit   = macro MacrosModule.implRBBC
def barRWBC: Unit   = macro MacrosModule.implRWBC
def barSRBBC: Unit  = macro MacrosModule.implSRBBC
def barSRWBC: Unit  = macro MacrosModule.implSRWBC
def barRSRBBC: Unit = macro MacrosModule.implRSRBBC
def barRSRWBC: Unit = macro MacrosModule.implRSRWBC

// barBBC
// barWBC
// barRBBC
// barRWBC
// barSRBBC
// barSRWBC
// barRSRBBC
// barRSRWBC


class MacroBundleBBC(val c: blackbox.Context)                                { def impl = c.universe.reify(()) }
class MacroBundleWBC(val c: whitebox.Context)                                { def impl = c.universe.reify(()) }
class MacroBundleRBBC(val c: reflect.macros.blackbox.Context)                { def impl = c.universe.reify(()) }
class MacroBundleRWBC(val c: reflect.macros.whitebox.Context)                { def impl = c.universe.reify(()) }
class MacroBundleSRBBC(val c: scala.reflect.macros.blackbox.Context)         { def impl = c.universe.reify(()) }
class MacroBundleSRWBC(val c: scala.reflect.macros.whitebox.Context)         { def impl = c.universe.reify(()) }
class MacroBundleRSRBBC(val c: _root_.scala.reflect.macros.blackbox.Context) { def impl = c.universe.reify(()) }
class MacroBundleRSRWBC(val c: _root_.scala.reflect.macros.whitebox.Context) { def impl = c.universe.reify(()) }

def bazBBC: Unit    = macro MacroBundleBBC.impl
def bazWBC: Unit    = macro MacroBundleWBC.impl
def bazRBBC: Unit   = macro MacroBundleRBBC.impl
def bazRWBC: Unit   = macro MacroBundleRWBC.impl
def bazSRBBC: Unit  = macro MacroBundleSRBBC.impl
def bazSRWBC: Unit  = macro MacroBundleSRWBC.impl
def bazRSRBBC: Unit = macro MacroBundleRSRBBC.impl
def bazRSRWBC: Unit = macro MacroBundleRSRWBC.impl
//
// bazBBC
// bazWBC
// bazRBBC
// bazRWBC
// bazSRBBC
// bazSRWBC
// bazRSRBBC
// bazRSRWBC
  """
}
