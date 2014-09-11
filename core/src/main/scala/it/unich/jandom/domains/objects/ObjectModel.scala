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
   * It returns true iff `t1` is a subtype of `t2`, i.e., if each variable of type `t1`
   * may be saved in a variable of type `t2`.
   */
  def lteq(t1: Type, t2: Type): Boolean
  
  /**
   * If there is a type `t` which is subtype of all types in `ts`, then it returns a
   * type `t'` such that t' <= t. Otherwise, it returns `None`. If the language is endowed 
   * with intersection types, glb should probably be it.
   */
  def glbApprox(ts: Iterable[Type]): Option[Type]
  
  /**
   * This returns true iff two variables of type t1 and t2 may share.
   */
  def mayShare(t1: Type, t2: Type): Boolean

  /**
   * This returns true iff two variables of type t1 and t2 may be aliased.
   */
  def mayBeAliases(t1: Type, t2: Type): Boolean
  
  /**
   * It returns the set of all the fields possessed by an object of type `t`.
   */
  def fieldsOf(t: Type): Set[Field]

  /**
   * It returns the type of the object pointed by field `f`.
   */
  def typeOf(f: Field): Type

  /**
   * Returns whether a type `t` is an array.
   */
  def isArray(t: Type): Boolean

  /**
   * Returns the element type of the array `t`, or `None` if `t` is not an array
   */
  def getElementType(t: Type): Option[Type]

}

/**
 * This is the companion class for ObjectModel. It collects several traits useful in building
 * trivial object models, especially in test suites.
 */
object ObjectModel {

  /**
   * This is a trivial object model with a single type with no fields and no arrays.
   */
  object Trivial extends ObjectModel with NoArrays {
    self: ObjectModel =>
    type Type = Unit
    type Field = Int
    def mayShare(t1: Type, t2: Type) = true
    def mayBeAliases(t1: Type, t2: Type) = true
    def fieldsOf(t: Type) = Set()
    def typeOf(f: Field) = {}
    def lteq(t1: Type, t2: Type) = true
    def glbApprox(ts: Iterable[Type]) = Some(())
  }

  /**
   * This is a trait for object models without arrays
   */
  trait NoArrays {
    self: ObjectModel =>
    def isArray(t: Type) = false
    def getElementType(t: Type) = None
  }
}
