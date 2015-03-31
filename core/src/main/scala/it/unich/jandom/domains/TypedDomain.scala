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

package it.unich.jandom.domains

/**
 * A typed domain is a domain whose properties are meaningful only in the context
 * of a type environment. They are similar to fibered domain, but updating the type
 * environment is not the duty of the domain. Instead, the type environment is updating
 * externally, and changes are communicated to the property. It may be used when
 * different analysis use the same type environment, so that we do not keep a different
 * copy of it in each domain. For the moment it is not used.
 * @author Gianluca Amato <gamato@unich.it>
 */
trait TypedDomain extends AbstractDomain {

  /**
   * The type environment connected to this abstract domain
   */
  type TypeEnvironment <: AbstractTypeEnvironment

  /**
   * The property connected to this domain
   */
  type Property <: TypedProperty[Property]

  /**
   * Returns the top element on the given type environment.
   */
  def top(te: TypeEnvironment): Property

  /**
   * Returns the bottom element on the given type environment.
   */
  def bottom(te: TypeEnvironment): Property

  /**
   * The property corresponding to a typed domain. The specification is not finished. An
   * important point that should be cleared is whether the type environment is pointed by
   * the property or not. In the last case, it should be passed as a parameter at every
   * call.
   * @author Gianluca Amato <gamato@unich.it>
   *
   */
  trait TypedProperty[P <: TypedProperty[P]] <: AbstractProperty[P] {

  }
}
