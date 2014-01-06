/** This tests that hyperlinking works for super types. See SI-7224 */
class BadPos[A](a: A)

class Base

trait Trait extends Base
trait SubTrait extends Trait
trait LateralTrait extends Base

object obj1 extends BadPos/*#*/(new Object)
object obj2 extends BadPos/*#*/[AnyRef](new Object)
object obj3 extends Trait/*#*/
object obj4 extends SubTrait/*#*/
object obj5 extends Trait/*#*/ with LateralTrait/*#*/
object obj6 extends Base/*#*/ with Trait/*#*/ with LateralTrait/*#*/

class PBase[A]

trait PTrait[A] extends PBase/*#*/[A]
trait PSubTrait[A] extends PTrait/*#*/[A]
trait PLateralTrait[A] extends PBase/*#*/[A]

object pobj2 extends PTrait/*#*/[Int]
object pobj3 extends PSubTrait/*#*/[Int]
object pobj4 extends PTrait/*#*/[Int] with PLateralTrait/*#*/[Int]
object pobj5 extends PBase/*#*/[Int] with PTrait/*#*/[Int] with PLateralTrait/*#*/[Int]

class c1 extends BadPos/*#*/(new Object)
class c2 extends PTrait/*#*/[Int]
class c3 extends PSubTrait/*#*/[Int]
class c4 extends PTrait/*#*/[Int] with PLateralTrait/*#*/[Int]
class c5 extends PBase/*#*/[Int] with PTrait/*#*/[Int] with PLateralTrait/*#*/[Int]
