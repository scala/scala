import scala.reflect.macros.Context
import java.io.InputStream

object Macros {
   def unpack[A](input: InputStream): A = macro unpack_impl[A]

   def unpack_impl[A: c.AbsTypeTag](c: Context)(input: c.Expr[InputStream]): c.Expr[A] = {
     import c.universe._

     def unpackcode(tpe: c.Type): c.Expr[_] = {
       if (tpe <:< implicitly[c.AbsTypeTag[Traversable[_]]].tpe) {

       }
       ???
     }

     unpackcode(implicitly[c.AbsTypeTag[A]].tpe)
     ???
   }
 }