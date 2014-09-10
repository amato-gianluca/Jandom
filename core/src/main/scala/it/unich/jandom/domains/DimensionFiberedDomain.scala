/**
 * Copyright 2013 Gianluca Amato <gamato@unich.it>
 *
 * This file is part of JANDOM: JVM-based Analyzer for Numerical DOMains
 * JANDOM is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * JANDOM is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty ofa
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with JANDOM.  If not, see <http://www.gnu.org/licenses/>.
 */

package it.unich.jandom.domains

/**
 * A `DimensionFiberedDomain` is a fibered untyped domain. Hence, each fiber is
 * characterized entirely by its dimension.
 * @author Gianluca Amato <gamato@unich.it>
 */

trait DimensionFiberedDomain extends CartesianFiberedDomain {
  type FiberType = Unit

  type Property <: DimensionFiberedProperty[Property]

  /**
   * Returns the top element of the given dimension.
   */
  def top(dimension: Int): Property

  /**
   * Returns the bottom element of the given dimension.
   */
  def bottom(dimension: Int): Property

  def top(f: Seq[FiberType]) = top(f.length)

  def bottom(f: Seq[FiberType]) = bottom(f.length)
}