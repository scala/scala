//

object O { val a = 1 }

import O.{a, a}   // error
import O.{a => b, a}  // ok
import O.{a, a => b}  // ok
import O.{a => b, a => b}   // error
import O.{a => b, a => c}   // ok
import O.{a => b, toString => b}    // error
import O.{a => toString, toString}  // error
import O.{toString, a => toString}  // error
import O.{a => _, toString => _}    // ok
import O.{given, a, _}    // ok
import O.{given, toString, a, _}    // ok
import O.{a, given, *}    // ok
import O.{a, *, given}    // ok
import O.{a, given, *, _} // ok
import O.{a, given}       // ok
