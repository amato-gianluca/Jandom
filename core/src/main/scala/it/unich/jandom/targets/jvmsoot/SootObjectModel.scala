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

package it.unich.jandom.targets.jvmsoot

import it.unich.jandom.domains.objects.ObjectModel
import soot.RefType
import soot.ArrayType
import soot.PrimType

/**
 * An object model for JVM using the Soot library.
 * @author Gianluca Amato <gamato@unich.it>
 */
class SootObjectModel(cra: SootClassReachableAnalysis) extends ObjectModel {
  import scala.collection.JavaConversions._

  val hierarchy = cra.scene.getOrMakeFastHierarchy()

  type Type = soot.Type
  type Field = soot.SootField
  def mayShare(i: Type, j: Type) =
    (i, j) match {
      case (p1: soot.RefType, p2: soot.RefType) => cra.mayShare(p1.getSootClass(), p2.getSootClass())
      case _ => false
    }

  def fieldsOf(i: Type) = i match {
    case i: soot.RefType => fieldsOf(i.getSootClass())
    case _ => Set()
  }

  private def fieldsOf(c: soot.SootClass): Set[Field] = {
    if (c.hasSuperclass())
      fieldsOf(c.getSuperclass()) ++ c.getFields.toSet
    else
      c.getFields.toSet
  }

  def typeOf(f: Field) = f.getType()

  def isArray(t: Type) = t.isInstanceOf[ArrayType]

  def isPrimitive(t: Type) = t.isInstanceOf[PrimType]

  def getElementType(t: Type) = t match {
    case t: ArrayType => Some(t.getElementType())
    case _ => None
  }

  /**
   * @inheritdoc
   * For the moment, we consider primitive types to be incomparable, but I do not know
   * if it is the correct way to handle this.
   */
  def lteq(t1: Type, t2: Type) =
    if (t1.isInstanceOf[soot.RefType] && t2.isInstanceOf[soot.RefType])
      hierarchy.canStoreType(t1, t2)
    else
      t1 == t2
}
