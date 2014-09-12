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
 * A typed domain depends from a type environment.
 * @author Gianluca Amato <gamato@unich.it>
 *
 */
trait TypedDomain extends AbstractDomain {

  /**
   * The real type environment connected to this abstract domain
   */
  type TypeEnvironment <: AbstractTypeEnvironment

  /**
   * The property connected to this domain
   */
  type Property <: TypedProperty[Property]

  /**
   * Returns the top element of the given type environment.
   */
  def top(te: TypeEnvironment): Property

  /**
   * Returns the bottom element of the given type environment.
   */
  def bottom(te: TypeEnvironment): Property

  /**
   * The property corresponding to a type domain has a filter method which improves
   * the abstraction using information from the type environment.
   * @author Gianluca Amato <gamato@unich.it>
   *
   */
  trait TypedProperty[Property <: TypedProperty[Property]] <: AbstractProperty[Property] {

    /**
     * Returns a more precise property, according to the information in the type environment.
     */
    def filter(te: TypeEnvironment): Property
  }
}


