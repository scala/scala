/* ************************************************************************
 * Pizza constant folder
 * by Martin Odersky
 *
 * Copyright (C) 1996,97 Martin Odersky. All rights reserved.
 * Permission is hereby granted to modify and use this software for research
 * and teaching purposes. Modification for commercial purposes requires
 * prior written permission by the author.
 * The software, or modifications thereof, may be redistributed only
 * if this copyright notice stays attached.
 *************************************************************************/

import scalac.{Global => scalac_Global}
import scalac.typechecker.{ConstantFolder => scalac_typechecker_ConstantFolder}

package scala.tools.scalac.typechecker {

class ConstantFolder(g: scalac_Global) extends scalac_typechecker_ConstantFolder(g) {
  // !!! rewrite
}

}
