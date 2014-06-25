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
import scala.collection.mutable
import soot.util.Chain
import soot.util.HashChain
import soot.{ Unit => SootUnit, _ }
import scala.collection.mutable.Queue

/**
 * An object model for JVM using the Soot library.
 * @author Gianluca Amato <gamato@unich.it>
 */
class SootObjectModel(scene: soot.Scene) extends ObjectModel {
  import scala.collection.JavaConversions._

  type Type = soot.Type

  type Field = soot.SootField

  val fh = scene.getOrMakeFastHierarchy()

  var reachable = mutable.HashMap[Type, Set[Type]]()
  var sharing = mutable.HashMap[(Type, Type), Boolean]()

  /**
   * Copy into result all the fields that an object of class/interface `ci` is
   * guaranteed to have.
   */
  private def addNeededFields(fields: mutable.Set[Field], ci: SootClass): Unit = {
    fields addAll ci.getFields()
    var current = ci
    while (current.hasSuperclass()) {
      current = current.getSuperclass()
      fields ++= current.getFields()
    }
  }

  /**
   * Returns all the fields that an object of class/interface `ci` is
   * guaranteed to have.
   */
  private def getNeededFields(ci: SootClass): Set[Field] = {
    val fields = new mutable.HashSet[Field]()
    addNeededFields(fields, ci)
    fields.toSet
  }

  /**
   * Returns all the fields that a type is guaranteed to have.
   */
  def getNeededFields(t: Type): Set[Field] = t match {
    case t: RefType => getNeededFields(t.getSootClass())
    case _: PrimType => Set()
    case _: ArrayType => Set()
  }

  /**
   * Returns all the fields that an object of class/interface `klass` may
   * possibly have.
   */
  private def getPossibleFields(ci: SootClass): Set[Field] = {
    val fields = new mutable.HashSet[Field]()
    addNeededFields(fields, ci)
    if (ci.isInterface) {
      val implementers = fh.getAllImplementersOfInterface(ci)
      for (c <- implementers) addNeededFields(fields, c)
    } else {
      val queue = Queue[SootClass]()
      queue.enqueue(fh.getSubclassesOf(ci).toSeq: _*)
      while (!queue.isEmpty) {
        val c = queue.dequeue
        fields ++= c.getFields
        queue.enqueue(fh.getSubclassesOf(c).toSeq: _*)
      }
    }
    fields.toSet
  }

  /**
   * Returns all the fields that a type may possibly have.
   */
  def getPossibleFields(t: Type): Set[Field] = t match {
    case t: RefType => getPossibleFields(t.getSootClass())
    case _: PrimType => Set()
    case _: ArrayType => Set()
  }

  /**
   * Determines whether a class or interface mat be instantiated, i.e. it has a concrete
   * subclass / implementer. This should be refined by removing those classes whose
   * constructor depends on non-instantiatable classes.
   */
  private def classMayBeInstantiated(ci: SootClass): Boolean = {
    if (ci.isConcrete)
      true
    else if (ci.isInterface() && (fh.getAllImplementersOfInterface(ci) exists { _.isConcrete }))
      true
    else {
      var c = ci
      val queue = Queue[SootClass]()
      queue.enqueue(fh.getSubclassesOf(ci).toSeq: _*)
      while (!queue.isEmpty || c.isConcrete) {
        val c = queue.dequeue
        queue.enqueue(fh.getSubclassesOf(c).toSeq: _*)
      }
      c.isConcrete()
    }
  }

  /**
   * Determines whether an object of type t (or one of its subtypes) may be
   * allocated on the heap.
   */
  def typeMayBeInstantiated(t: Type): Boolean = t match {
    case t: RefType => classMayBeInstantiated(t.getSootClass())
    case _: PrimType => false
    case t: ArrayType => true
  }

  /**
   * Determines an upper crown of the types reachable from a variable of declared type `t`.
   * It uses memoization to speed up subsequent calls. It only returns an upper crown of
   * all the reachable classes.
   */
  private def reachablesFrom(t: Type): Set[Type] = reachable.get(t) match {
    case Some(types) =>
      types
    case None =>
      reachable(t) = Set()
      val result: Set[Type] = t match {
        case t: RefType =>
          val klass = t.getSootClass()
          val result: Set[Type] = for {
            field <- getPossibleFields(klass)
            reachableTypes <- reachablesFrom(field.getType)
          } yield reachableTypes
          if (classMayBeInstantiated(klass))
            result + t
          else
            result
        case t: PrimType =>
          Set()
        case t: ArrayType =>
          reachablesFrom(t.getElementType()) + t
      }
      reachable(t) = result
      result
  }

  /**
   * Determines whether the type `tgt` is reachable from `src`.
   */
  def reachableFrom(src: Type, tgt: Type) =
    reachablesFrom(src) exists { fh.canStoreType(tgt, _) }

  def mayShare(t1: Type, t2: Type) = {
    val (tmin, tmax) = if (t1.getNumber() < t2.getNumber()) (t1, t2) else (t2, t1)
    sharing.get((t1, t2)) match {
      case Some(result) => result
      case None =>
        // TODO: it is possible to make this faster.
        val result = reachablesFrom(t1) exists { reachableFrom(t2, _) }
        sharing((tmin, tmax)) = result
        result
    }
  }
  
  def mayBeAliases(t1: Type, t2: Type) = glb(Seq(t1,t2)).isDefined
    
  def fieldsOf(t: Type) = getNeededFields(t)

  def glb(ts: Iterable[Type]) =
    if (ts.isEmpty)
      None
    else {
      var current: Option[Type] =
        if (ts.head.isInstanceOf[RefType] && ts.head.asInstanceOf[RefType].getSootClass().isInterface())
          Some(scene.getObjectType())
        else
          Some(ts.head)
      var it = ts.tail.iterator
      while (current != None && it.hasNext) {
        val t = it.next()
        if (t.isInstanceOf[RefType] && t.asInstanceOf[RefType].getSootClass().isInterface()) {
          if (!current.get.isInstanceOf[RefType])
            current = None
        } else if (lteq(t, current.get))
          current = Some(t)
        else if (! lteq(current.get, t))
          current = None
      }
      current
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
  def lteq(t1: Type, t2: Type) = fh.canStoreType(t1, t2)

}
