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

import it.unich.jandom.objectmodels.ObjectModel
import scala.collection.mutable
import soot.util.Chain
import soot.util.HashChain
import soot.{ Unit => SootUnit, _ }
import scala.collection.mutable.Queue
import soot.jandom.MyFastHierarchy
import org.hamcrest.core.IsInstanceOf
import it.unich.jandom.objectmodels.ObjectModelHelper

/**
 * An object model for JVM using the Soot library.
 * @author Gianluca Amato <gamato@unich.it>
 */
class SootObjectModel(scene: soot.Scene) extends ObjectModel with ObjectModelHelper {

  import scala.collection.JavaConversions._

  type Type = soot.Type

  type Field = soot.SootField

  /**
   * Returns a MyFastHierarchy, which is a SootFastHirearchy modified to expose direct subinterface
   * relationship.
   */
  val fh = if (scene.hasFastHierarchy() && scene.getFastHierarchy().isInstanceOf[MyFastHierarchy])
    scene.getFastHierarchy().asInstanceOf[MyFastHierarchy]
  else {
    val newfh = new MyFastHierarchy
    scene.setFastHierarchy(newfh)
    newfh
  }

  def declaredFields(t: Type) = t match {
    case t: RefType => t.getSootClass().getFields().toSet
    case _: PrimType => Set()
    case _: ArrayType => Set()
  }

  def typeOf(f: Field) = f.getType()

  def isPrimitive(t: Type) = t.isInstanceOf[PrimType]

  def isConcrete(t: Type) = t match {
    case t: RefType => t.getSootClass().isConcrete()
    case _: PrimType => true
    case _: ArrayType => true
  }

  /**
   * @inheritdoc
   * For the moment, we consider primitive types to be incomparable, but I do not know
   * if it is the correct way to handle this.
   */
  def lteq(t1: Type, t2: Type) = fh.canStoreType(t1, t2)

  def parents(t: Type) = t match {
    case t: RefType =>
      val k = t.getSootClass()
      val ifs: Set[Type] = (for {
        i <- k.getInterfaces()
      } yield i.getType())(collection.breakOut)
      if (k.hasSuperclass())
        ifs + k.getSuperclass().getType()
      else
        ifs
    case _: PrimType => Set()
    case t: ArrayType => parents(t.baseType) map { (ArrayType.v(_, t.numDimensions)) }
  }

  def children(t: Type) = t match {
    case t: RefType =>
      val k = t.getSootClass()
      if (k.isInterface()) {
        (fh.getAllImplementersOfInterface(k) map { _.getType() }).toSet ++
          (fh.getSubinterfaces(k) map { _.getType() }).toSet
      } else {
        (fh.getSubclassesOf(k) map { _.getType() }).toSet
      }
    case _: PrimType => Set()
    case t: ArrayType => children(t.baseType) map { (ArrayType.v(_, t.numDimensions)) }
  }

  def isArray(t: Type) = t.isInstanceOf[ArrayType]

  def getElementType(t: Type) = t match {
    case t: ArrayType => Some(t.baseType)
    case _ => None
  }

  /**
   * Returns whether a type is an interface.
   */
  def isInterface(t: Type) =
    t.isInstanceOf[RefType] && t.asInstanceOf[RefType].getSootClass().isInterface()

  /**
   * This is a fast approximation of glbApprox which do not consider concretizability
   * of a type and makes all interfaces equivalent to the Object type. It should be always
   * a super-type of glbApprox. Moreover, if it is undefined, glbApprox should be undefined too.
   */
  def glbApproxFast(t1: Type, t2: Type): Option[Type] = {
    val tt1 = if (isInterface(t1))
      scene.getObjectType()
    else t1
    val tt2 = if (isInterface(t2))
      scene.getObjectType()
    else t2
    if (lteq(tt1, tt2))
      Some(tt1)
    else if (lteq(tt2, tt1))
      Some(t2)
    else
      None
  }
}
