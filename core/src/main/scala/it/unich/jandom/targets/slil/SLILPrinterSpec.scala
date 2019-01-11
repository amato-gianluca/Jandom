/**
  * Copyright 2013, 2018 Gianluca Amato
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

package it.unich.jandom.targets.slil

import it.unich.jandom.domains.numerical.NumericalProperty
import it.unich.jandom.targets.Environment

/**
 * A SLILPrinterSpec help in printing a SLIL program with accompanying assertions.
 * @author Gianluca Amato <gamato@unich.it>
 */
abstract class SLILPrinterSpec {
  /**
   * The number of spaces for each indentation level.
   */
  val indentWidth: Int

  /**
   * An environment used to print properties.
   */
  def env: Environment

  /**
   * Generates the correct number of spaces for the given indentation level.
   */
  def indent(level: Int): String = { " " * level * indentWidth }

  /**
   * Decorator is a function which takes a numerical property and returns its string representation.
   * @param p the property to output.
   * @param row the row of the program which p refers to.
   * @param col the column of the program which p refers to
   */
  def decorator (p: NumericalProperty[_], row: Int, col: Int): String
}
