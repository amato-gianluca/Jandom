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
import it.unich.jandom.domains.objects.UP
import scala.collection.mutable
import soot._

/**
 * An object model for JVM using the Soot library.
 * @author Gianluca Amato <gamato@unich.it>
 */
class SootObjectModel(scene: soot.Scene) extends ObjectModel {
  import scala.collection.JavaConversions._

  type Type = soot.Type

  type Field = soot.SootField

  implicit object TypeOrdering extends Ordering[Type] {
    def compare(x: Type, y: Type) = x.getNumber() - y.getNumber()
  }

  val fh = scene.getOrMakeFastHierarchy()

  var reachable = new mutable.HashMap[Type, Set[Type]]
  var sharing = new mutable.HashMap[UP[Type], Boolean]

  private def getAllSuperClasses(c: soot.SootClass, acc: Seq[soot.SootClass] = Seq()): Seq[soot.SootClass] = {
    if (c.hasSuperclass())
      getAllSuperClasses(c.getSuperclass(), c +: acc)
    else
      c +: acc
  }

  private def getAllSubClassesNoInterface(klass: soot.SootClass): Set[soot.SootClass] = {
    val oneLevel = fh.getSubclassesOf(klass).asInstanceOf[java.util.Collection[SootClass]]
    val allLevels: Set[soot.SootClass] = (oneLevel flatMap getAllSubClassesNoInterface)(collection.breakOut)
    allLevels + klass
  }

  def getAllSubClasses(klass: soot.SootClass): Set[soot.SootClass] =
    if (klass.isInterface()) {
      val implementers = fh.getAllImplementersOfInterface(klass).asInstanceOf[java.util.Set[SootClass]].toSet
      //val subint = fh.getAllSubinterfaces(klass).asInstanceOf[java.util.Set[SootClass]].toSet
      implementers
    } else
      getAllSubClassesNoInterface(klass)

  private def getAllSubArrays(arr: soot.ArrayType): Set[soot.ArrayType] = {
    arr.baseType match {
      case p: PrimType =>
        Set(arr)
      case r: RefType =>
        val subClasses = getAllSubClasses(r.getSootClass)
        subClasses map { c => ArrayType.v(RefType.v(c), arr.numDimensions) }
      case _ =>
        throw new IllegalStateException("Not implemented yet... I do not known what it means")
    }
  }

  /**
   * Determines all the classes reachable from a variable of declared type `klass`. It uses memoization
   * to speed up subsequent calls. It only returns an upper crown of all the reachable classes.
   */
  private def reachablesFrom(t: Type): Set[Type] = reachable.get(t) match {
    case Some(t) =>
      t
    case None =>
      reachable(t) = Set()
      val allButMe: Set[Type] = t match {
        case t: RefType =>
          (for {
            fieldSource <- getAllSuperClasses(t.getSootClass()) ++ getAllSubClasses(t.getSootClass())
            field <- fieldSource.getFields()
            reachableTypes <- reachablesFrom(field.getType)
          } yield reachableTypes)(collection.breakOut)
        case t: PrimType =>
          Set()
        case t: ArrayType =>
          Set(t.baseType)
      }
      val all = allButMe + t
      reachable(t) = all
      all
  }

  /**
   * Determines whether the class `tgt` is reachable from `src`.
   */
  def reachableFrom(src: Type, tgt: Type) =
    reachablesFrom(src) exists { fh.canStoreType(tgt, _) }

  def mayShare(t1: Type, t2: Type) = {
    sharing.get(UP(t1, t2)) match {
      case Some(result) => result
      case None =>
        // TODO: it is possible to make this faster.
        val result = reachablesFrom(t1) exists { reachablesFrom(t2) contains _ }
        sharing(UP(t1, t2)) = result
        result
    }
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
      fh.canStoreType(t1, t2)
    else
      t1 == t2
}
