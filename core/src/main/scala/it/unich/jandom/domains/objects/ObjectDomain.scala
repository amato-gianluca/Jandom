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

package it.unich.jandom.domains.objects

import it.unich.jandom.domains.CartesianFiberedDomain
import it.unich.jandom.domains.CartesianFiberedProperty

/**
 * This trait represents the interface for a domain which handles objects and their relationship.
 * May be used, for example, for sharing analysis. This is only a draft, and will be probably improved
 * along the development of Jandom.
 * @tparam OM the particular type of object model related to the object domain
 * @author Gianluca Amato <gamato@unich.it>
 *
 */
trait ObjectDomain[OM <: ObjectModel] extends CartesianFiberedDomain {

  /**
   * The object model of this domain
   */
  val om: OM

  type Property <: ObjectProperty[Property]

  /**
   * The type of the fiber components is `om.Type`.
   */
  type FiberComponent = om.Type

  /**
   * This trait is the interface for abstract elements in the object domain.
   */
  trait ObjectProperty[P <: ObjectProperty[P]] extends CartesianFiberedProperty[om.Type, P] {
    this: P =>

    /**
     * Add a new variable. The new variable may be in whatever relationship with the
     * old ones.
     * @param t the type of the new variable
     */
    def addUnknownVariable(t: om.Type): P

    /**
     * Add a new non-null variable which does not share with any other variable.
     * @param t the type of the new variable
     */
    def addFreshVariable(t: om.Type): P

    /**
     * Assign the null object to variable `dst`.
     */
    def assignNull(dst: Int = dimension - 1): P

    /**
     * Corresponds to the assignment `dst = src`. We assume dst is a subtype
     * of src
     */
    def assignVariable(dst: Int, src: Int): P

    /**
     * Corresponds to the assignment `dst.field = src`. We assume dst.field is a subtype
     * of src
     */
    def assignVariableToField(dst: Int, field: om.Field, src: Int): P

    /**
     * Corresponds to the assignment `dst = src.field`. We assume dst is a subtype of src
     */
    def assignFieldToVariable(dst: Int, src: Int, field: om.Field): P

    /**
     * Change the type of variable i-th. We assume the new type is comparable with the old one.
     */
    def castVariable(v: Int, newtype: om.Type): P

    /**
     * Returns true if the location obtained by v following fields in fieldseq is definitively
     * null. If some intermediate value is definitively null, it returns true.
     */
    def mustBeNull(v: Int, fieldseq: Seq[om.Field] = Seq()): Boolean

    /**
     * Returns true if the location obtained by v following fields in fieldseq may be
     * null. If some intermediate value is definitively null, it returns true.
     */
    def mayBeNull(v: Int, fieldseq: Seq[om.Field] = Seq()): Boolean

    /**
     * Returns true if two variables may share
     */
    def mayShare(v1: Int, v2: Int): Boolean

    /**
     * Returns true if two variables must share
     */
    def mustShare(v1: Int, v2: Int): Boolean

    /**
     * Returns true if two variables may be aliases
     */
    def mayBeAliases(v1: Int, v2: Int): Boolean

    /**
     * Returns true if two variables must be aliases
     */
    def mustBeAliases(v1: Int, v2: Int): Boolean

    /**
     * Returns the property after the successful completion of the test `v == null`
     */
    def testNull(v: Int): P

    /**
     * Returns the property after the successful completion of the test `v != null`
     */
    def testNotNull(v: Int): P
  }
}
