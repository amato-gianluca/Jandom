/**
 * Copyright 2014 Gianluca Amato <gamato@unich.it>
 *
 * This file is part of JANDOM: JVM-based Analyzer for Numerical DOMains
 * JANDOM is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * JANDOM is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of a
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with JANDOM.  If not, see <http://www.gnu.org/licenses/>.
 */

package it.unich.jandom.domains.objects

import it.unich.jandom.domains.AbstractTypeEnvironment

/**
 * This is a type environment which describes all the information about
 * pair sharing which is discoverable just by type checking.
 * @author Gianluca Amato <gamato@unich.it>
 */
trait ObjectTypeEnvironment <: AbstractTypeEnvironment {
  /**
   * Number of variables in the environment
   */
  def numvars: Int

  /**
   * The result is `true` if `i` and `j` may share due to type checking.
   */
  def mayShare(i: Int, j: Int): Boolean

  /**
   * This returns true if variable `v` may be non-linear
   */
  def mayNonLinear(v: Int): Boolean
}
