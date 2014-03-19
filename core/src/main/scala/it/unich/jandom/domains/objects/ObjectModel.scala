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

/**
 * An object model encapsulate all the informations on the object model of a language.
 * It also encapsulated differences on the low level libraries used to interpret
 * programs (such as Soot vs ASM vs ..)
 * @author Gianluca Amato <gamato@unich.it>
 *
 */
trait ObjectModel {
  /**
   * This is the type for variables
   */
  type Variable

  /**
   * This is the type for types
   */
  type Type

  /**
   * This is the type for fields. Two fields are the same only if they are defined
   * in the same point of the type hierarchy.
   */
  type Field

  /**
   * This returns true if two variable of type src and tgt may share
   */
  def mayShare(src: Type, tgt: Type): Boolean

  /**
   * It returns the set of all the fields of the type `t`.
   */
  def fieldsOf(t: Type): Set[Field]

  /**
   * It returns the type of the object pointed by type `t`
   */
  def typeOf(f: Field): Type

  /**
   * It returns true if `t1` is a subtype of `t2`. Each type is a subtype
   * of itself.
   */
  def lteq(t1: Type, t2: Type): Boolean

  /**
   * It returns the minimum between t1 and t2 if it exists, and returns
   * an exception otherwise.
   */
  def min(t1: Type, t2: Type) =
    if (lteq(t1, t2)) t1
    else if (lteq(t2, t1)) t2
    else throw new IllegalArgumentException("The min method of ObjectModel only accepts comparable types")
}
