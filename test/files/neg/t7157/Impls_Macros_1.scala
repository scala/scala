import scala.reflect.macros.blackbox.Context
import language.experimental.macros

object Macros {
  def impl1_0_0(c: Context)() = { import c.universe._; c.Expr[Unit](q"""println("hello world")""") }
  def impl1_1_1(c: Context)(x: c.Expr[Int]) = { import c.universe._; c.Expr[Unit](q"""println("hello world")""") }
  def impl1_2_2(c: Context)(x: c.Expr[Int], y: c.Expr[Int]) = { import c.universe._; c.Expr[Unit](q"""println("hello world")""") }
  def m1_0_0() = macro impl1_0_0
  def m1_1_1(x: Int) = macro impl1_1_1
  def m1_2_2(x: Int, y: Int) = macro impl1_2_2

  def impl1_0_inf(c: Context)(x: c.Expr[Int]*) = { import c.universe._; c.Expr[Unit](q"""println("hello world")""") }
  def impl1_1_inf(c: Context)(x: c.Expr[Int], y: c.Expr[Int]*) = { import c.universe._; c.Expr[Unit](q"""println("hello world")""") }
  def impl1_2_inf(c: Context)(x: c.Expr[Int], y: c.Expr[Int], z: c.Expr[Int]*) = { import c.universe._; c.Expr[Unit](q"""println("hello world")""") }
  def m1_0_inf(x: Int*) = macro impl1_0_inf
  def m1_1_inf(x: Int, y: Int*) = macro impl1_1_inf
  def m1_2_inf(x: Int, y: Int, z: Int*) = macro impl1_2_inf

  def impl2_0_0(c: Context)()() = { import c.universe._; c.Expr[Unit](q"""println("hello world")""") }
  def impl2_1_1(c: Context)()(x: c.Expr[Int]) = { import c.universe._; c.Expr[Unit](q"""println("hello world")""") }
  def impl2_2_2(c: Context)()(x: c.Expr[Int], y: c.Expr[Int]) = { import c.universe._; c.Expr[Unit](q"""println("hello world")""") }
  def m2_0_0()() = macro impl2_0_0
  def m2_1_1()(x: Int) = macro impl2_1_1
  def m2_2_2()(x: Int, y: Int) = macro impl2_2_2

  def impl2_0_inf(c: Context)()(x: c.Expr[Int]*) = { import c.universe._; c.Expr[Unit](q"""println("hello world")""") }
  def impl2_1_inf(c: Context)()(x: c.Expr[Int], y: c.Expr[Int]*) = { import c.universe._; c.Expr[Unit](q"""println("hello world")""") }
  def impl2_2_inf(c: Context)()(x: c.Expr[Int], y: c.Expr[Int], z: c.Expr[Int]*) = { import c.universe._; c.Expr[Unit](q"""println("hello world")""") }
  def m2_0_inf()(x: Int*) = macro impl2_0_inf
  def m2_1_inf()(x: Int, y: Int*) = macro impl2_1_inf
  def m2_2_inf()(x: Int, y: Int, z: Int*) = macro impl2_2_inf
}