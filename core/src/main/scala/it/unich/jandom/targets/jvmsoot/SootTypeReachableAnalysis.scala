/**
 * Copyright 2013, 2014 Gianluca Amato <gamato@unich.it>
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

package it.unich.jandom.targets.jvmsoot

import scala.collection.mutable

import it.unich.jandom.domains.objects.UP

import soot._

/**
 * This class analyzes a Soot Scene, looking for relationships of reachability
 * and sharing among classes.
 * @param scene the Scene for the analysis
 * @author Gianluca Amato <gamato@unich.it>
 */

class SootTypeReachableAnalysis(val scene: Scene) {
  import scala.collection.JavaConversions._

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

  private def getAllSubClasses(klass: soot.SootClass): Set[soot.SootClass] = {
    val oneLevel = fh.getSubclassesOf(klass).asInstanceOf[java.util.Collection[SootClass]]
    val allLevels: Set[soot.SootClass] = (oneLevel flatMap getAllSubClasses)(collection.breakOut)
    allLevels + klass
  }

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

  private def getMinimalSubTypes(tpe: Type): Set[_ <: soot.RefLikeType] = tpe match {
    case tpe: RefType => getAllSubClasses(tpe.getSootClass) map { RefType.v(_) }
    case tpe: ArrayType => getAllSubArrays(tpe)
    case tpe: PrimType => Set()
    case _ => throw new IllegalStateException("Not implemented yet... I do not known what it means")
  }

  /**
   * Determines all the classes reachable from a variable of declared type `klass`. It uses memoization
   * to speed up subsequent calls. It only returns an upper crown of all the reachable classes.
   */
  def reachablesFrom(t: Type): Set[Type] = reachable.get(t) match {
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
          } yield reachableTypes) (collection.breakOut)
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
  def isReachableFrom(src: RefLikeType, tgt: RefLikeType) = reachablesFrom(src) contains tgt

  /**
   * Determines whether two class may share. It uses memoization to speed up subsequent calls.
   * @note it is possible to speed up this analysis by recording not all the classes reachable
   * from a given one, but just some selected classes in reachability loops or classes with no
   * fields.
   */
  def mayShare(klass1: Type, klass2: Type) = sharing.get(UP(klass1, klass2)) match {
    case Some(result) => result
    case None =>
      // TODO: it is possible to make this faster.
      val result = reachablesFrom(klass1) exists { reachablesFrom(klass2) contains _ }
      sharing(UP(klass1, klass2)) = result
      result
  }
}
