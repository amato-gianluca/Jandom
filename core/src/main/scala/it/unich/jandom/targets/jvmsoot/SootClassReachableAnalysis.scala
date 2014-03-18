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

package it.unich.jandom.targets.jvmsoot

import scala.annotation.tailrec
import it.unich.jandom.domains.objects.UP
import soot._

/**
 * This class analyzes a Soot Scene, looking for relationships of reachability
 * and sharing among classes.
 * @param scene the Scene for the analysis
 * @author Gianluca Amato <gamato@unich.it>
 *
 */
class SootClassReachableAnalysis(val scene: Scene) {
  import scala.collection.JavaConversions._

  implicit object SootClassOrdering extends Ordering[SootClass] {
    def compare(x: SootClass, y: SootClass) = x.getNumber() - y.getNumber()
  }

  val fh = scene.getOrMakeFastHierarchy()

  var reachable = new scala.collection.mutable.HashMap[SootClass, Set[SootClass]]
  var sharing = new scala.collection.mutable.HashMap[UP[SootClass], Boolean]
  
  def getAllSuperClasses(c: soot.SootClass, acc: Seq[soot.SootClass] = Seq()): Seq[soot.SootClass] = {
    if (c.hasSuperclass())
      getAllSuperClasses(c.getSuperclass(), c +: acc)
    else
      c +: acc
  }

   def getAllSubClasses(klass: soot.SootClass): Set[soot.SootClass] = {
    val oneLevel = fh.getSubclassesOf(klass).asInstanceOf[java.util.Collection[SootClass]]
    val allLevels: Set[soot.SootClass] = (oneLevel flatMap getAllSubClasses)(collection.breakOut)
    allLevels + klass
  }

   def getAllReferredClasses(tpe: Type): Set[soot.SootClass] = tpe match {
    case tpe: RefType => getAllSubClasses(tpe.getSootClass())
    case tpe: ArrayType => getAllSubClasses(soot.RefType.v("java.lang.Object").getSootClass())
    case tpe: AnySubType => throw new IllegalStateException("Not implemented yet... I do not known what it means")
    case _ => Set()
  }

  /**
   * Determines all the classes reachable from a variable of declared type `klass`. It uses memoization
   * to speed up subsequent calls.
   */
  def reachablesFrom(klass: SootClass): Set[SootClass] =
    for {
      subclass <- getAllSubClasses(klass)
      reachable <- reachablesFromEffective(subclass)
    } yield reachable

  /**
   * Determines all the classes reachable from a variable of effective type `klass`. It uses memoization
   * to speed up subsequent calls.
   */
  def reachablesFromEffective(klass: SootClass): Set[SootClass] = {
    reachable.get(klass) match {
      case Some(klasses) => klasses
      case None =>
        reachable(klass) = Set()
        val reachableClasses: Set[SootClass] = (for {
          superclass <- getAllSuperClasses(klass)
          field <- superclass.getFields()
          refClass <- getAllReferredClasses(field.getType)
          reachableClass <- reachablesFromEffective(refClass)
        } yield reachableClass)(collection.breakOut)
        val res = reachableClasses + klass
        reachable(klass) = res
        res
    }
  }

  /**
   * Determines whether the class `tgt` is reachable from `src`.
   */
  def isReachableFrom(src: SootClass, tgt: SootClass) = reachablesFrom(src) contains tgt

  /**
   * Determines whether two class may share. It uses memoization to speed up subsequent calls.
   * @note it is possible to speed up this analysis by recording not all the classes reachable
   * from a given one, but just some selected classes in reachability loops or classes with no
   * fields.
   */
  def mayShare(klass1: SootClass, klass2: SootClass) = sharing.get(UP(klass1, klass2)) match {
    case Some(result) => result
    case None =>
      // TODO: it is possible to make this faster.
      val result = reachablesFrom(klass1) exists { reachablesFrom(klass2) contains _ }
      sharing(UP(klass1, klass2)) = result
      result
  }
}
