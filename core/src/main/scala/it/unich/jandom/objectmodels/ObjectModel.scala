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

package it.unich.jandom.objectmodels

import scala.annotation.tailrec

/**
 * An ObjectModel encapsulate all the informations on the object model of a language.
 * It also encapsulated differences on the low level libraries used to interpret
 * programs (such as Soot vs ASM vs ..). This trait has some abstract methods which
 * should be implemented in concrete classes, and some concrete methods which computes
 * a lor of relevant information on the object models. Concrete methods are not
 * particularly fast at the moment, since correctness and readability has been
 * favored rather than performance.
 * @todo make the concrete methods faster
 * @author Gianluca Amato <gamato@unich.it>
 */
trait ObjectModel {

  /********** ABSTRACT MEMBERS **********/

  /**
   * This is the abstract type for types in target language. A type may be concrete/abstract, primitive/non-primitive.
   * A concrete type may be instantiated, while an abstract one cannot. A primitive types is memorized in the stack
   * or local area of a method, a non-primitive is memorized on the heap.
   */
  type Type

  /**
   * This is the abstract type for fields in the target language.
   */
  type Field

  /**
   * It returns the set of all the fields declared by type `t` (not by its super- and sub-types). The result
   * of `fieldsOf` for different types should be disjoint.
   */
  def declaredFields(t: Type): Set[Field]

  /**
   * It returns the type of the field `f`.
   */
  def typeOf(f: Field): Type

  /**
   * Returns whether a type `t` is primitive. A primitive object is not memorized
   * on the heap, hence cannot be aliased.
   */
  def isPrimitive(t: Type): Boolean

  /**
   * Returns whether a type `t` is concrete. A concrete object may exists somewhere
   * in memory, otherwise it is abstract and cannot be instantiated.
   */
  def isConcrete(t: Type): Boolean

  /**
   * Returns the parents of a given type
   */
  def parents(t: Type): Set[Type]

  /**
   * Returns the children of a given type
   */
  def children(t: Type): Set[Type]

  /**
   * It returns true iff `t1` is a subtype of `t2`, i.e., if each variable of type `t1`
   * may be saved in a variable of type `t2` without conversion.
   */
  def lteq(t1: Type, t2: Type): Boolean

  /**
   * Returns whether a type `t` is an array. Array types are particular types which may be indexed with
   * natural numbers and returns element of a given type. No other assumption is made.
   */
  def isArray(t: Type): Boolean

  /**
   * Returns the element type of the array `t`, or `None` if `t` is not an array type.
   */
  def getElementType(t: Type): Option[Type]

  /********** CONCRETE MEMBERS **********/

  /**
   * Determines whether the sequence of fields `fs`, starting from an object of
   * type `t`, may be typed correctly.
   */
  @tailrec
  final def pathExists(t: Type, fs: Field*): Boolean = {
    if (fs.isEmpty)
      true
    else {
      val f = fs.head
      (fields(t) contains f) && pathExists(t, fs.tail: _*)
    }
  }

  /**
   * An helper method for a BFS visit of a graph. While it visit nodes, it accumulates the sets
   * returned by `value` and returns the result.
   * @param start the starting node for the visit
   * @param next a map from a node to thet set of nodes connected by its outgoing edges
   * @param value the map from a node to a set of values which are collected together
   * @todo make it work for any collecition types, non only sets.
   */
  protected def visitGraph[Node, Value](start: Node, next: Node => Set[Node], value: Node => Set[Value]): Set[Value] = {
    val worklist = collection.mutable.Queue[Node](start)
    val result = collection.mutable.Set[Value]()
    while (!worklist.isEmpty) {
      val current = worklist.dequeue()
      result ++= value(current)
      worklist ++= next(current)
    }
    result.toSet
  }

  /**
   * A mutable HashMap used for memoizing sharing information.
   */
  private val sharing = collection.mutable.HashMap[(Type, Type), Boolean]()

  /**
   * A mutable HashMap used for memoizing reachability information.
   */
  private val reachable = collection.mutable.HashMap[Type, Set[Type]]()

  /**
   * A mutable HashMap used for memoizing glbs
   */
  private val glb = collection.mutable.HashMap[(Type, Type), Option[Type]]()

  /**
   * Returns the ancestors of type `t`,
   */
  def ancestors(t: Type): Set[Type] = visitGraph(t, parents, { (t: Type) => Set(t) })

  /**
   * Returns the descendants of type `t`.
   */
  def descendants(t: Type): Set[Type] = visitGraph(t, children, { (t: Type) => Set(t) })

  /**
   * Returns the set of all fields of a given type `t`, both declared and inherited.
   */
  def fields(t: Type): Set[Field] = visitGraph(t, parents, declaredFields)

  /**
   * Returns the set of all fields which an object of declared type `t` might possibly have.
   */
  def possibleFieldsOf(t: Type): Set[Field] = {
    for { k <- descendants(t); if isConcrete(k); f <- fields(k) }
      yield f
  }

  /**
   * Returns whether some subtype of t is concrete.
   */
  def isConcretizable(t: Type): Boolean = {
    descendants(t) exists isConcrete
  }

  /**
   * Returns an upper crown for a collection of types. An upper crown of `ts` is set `crown` subset of
   * `ts` such that each element in `ts` has an upper bound in `crown`.
   */  
  def upperCrown(ts: Iterable[Type]): Set[Type] =  {
    
    @tailrec    
    def upperCrownHelper(ts: Iterable[Type], acc: Set[Type]): Set[Type] = {
      if (ts.isEmpty) 
        acc
      else {
        val newt = ts.head
        val newacc = collection.mutable.Set[Type]()
        var toAdd = true
        for (t <- acc) {
          if (lteq(newt,t)) toAdd = false
          if ((! lteq(t,newt)) || t == newt) newacc += t
        }
        if (toAdd) newacc += newt
        upperCrownHelper(ts.tail, newacc.toSet)           
      }         
    }
    upperCrownHelper(ts, Set())
  }
  
  /**
   * Returns an approximation of the glb of types `t1` and `t2`. The result
   * is guaranteed to be the most specialized type which is a super-type of all
   * concrete lower bounds of `t1` and `t2`. The result is None iff `t1` and `t2`
   * have no concrete lower bounds.
   * @todo we should prove it works, I am not entirely sure
   */
  def glbApprox(t1: Type, t2: Type): Option[Type] = glb.getOrElseUpdate((t1, t2), {
    if (lteq(t1, t2) && isConcrete(t1))
      Option(t1)
    else {
      val glbs = upperCrown(children(t1) map { glbApprox(_, t2) } filter { _.isDefined } map { _.get })
      // Actually, we would need to remove elements from glbs which are subsumed by other elements.
      if (glbs.isEmpty)
        None
      else if (glbs.forall(_ == glbs.head))
        Option(glbs.head)
      else
        Option(t1)
    }
  })

  /**
   * An unary glbApprox such that `glbApprox(t)` is equivalent to `glbApprox(t,t)`. It is
   * only used in tests and for this reason is marked private. 
   */
  private[objectmodels] def glbApprox(t: Type): Option[Type] = {
    var subs = Set(t)
    var current = t
    do {
      current = subs.head
      subs = upperCrown((children(current) filter isConcretizable) map { glbApprox(_).get })
    } while (subs.size == 1 && (!isConcrete(current)))
    if (isConcretizable(current))
      Option(current)
    else
      None
  }

  /**
   * Returns the glb approximation of a sequence of types. It is  computed by iterating the 
   * binary glbApprox, but may be overriden for performance reasons.
   */
  def glbApprox(ts: Iterable[Type]): Option[Type] = {

    @tailrec
    def glbhelper(ts: Iterable[Type], current: Option[Type]): Option[Type] = {
      if (ts.isEmpty || current.isEmpty)
        current
      else
        glbhelper(ts.tail, glbApprox(current.get, ts.head))
    }

    if (ts.isEmpty)
      None
    else
      glbhelper(ts, Option(ts.head))
  }

  /**
   * Returns the set of all fields which an object of declared type `t` must have.
   */
  def neededFieldsOf(t: Type): Set[Field] = {
    val glb = glbApprox(t,t)
    if (glb.isEmpty) Set() else fields(glb.get)
  }

  /**
   * Determines an upper crown of the types reachable from a variable of declared type `t`.
   * It uses memoization to speed up subsequent calls. A type `t2` is reachable
   * from `t` if there is a path that start from an object of declared type `t` and ends
   * in an object of declared type `t2`.
   */
  def reachablesFrom(t: Type): Set[Type] = reachable.get(t) match {
    case Some(types) =>
      types
    case None =>
      reachable(t) = Set()
      val set = collection.mutable.Set[Type]()
      val queue = collection.mutable.Queue[Type](t)
      while (queue.nonEmpty) {
        val t1 = queue.dequeue
        if (isConcretizable(t1) && !isPrimitive(t1)) set += t1
        for { f <- possibleFieldsOf(t1); t2 = typeOf(f); if !set.contains(t2) } queue.enqueue(t2)
        for { elt <- getElementType(t1) } queue.enqueue(elt)
      }
      val result = set.toSet
      reachable(t) = result
      result
  }

  /**
   * Determines whether the type `tgt` is reachable from `src`. `tgt` may be
   * not concretizable.
   */
  def reachable(src: Type, tgt: Type) = reachablesFrom(src) exists { lteq(tgt, _) }

  /**
   * Returns true if two variables of type `t1` and `t2` may be aliases. Two variable are
   * aliases when they point to the same location in the heap.
   */
  def mayBeAliases(t1: Type, t2: Type): Boolean =
    !isPrimitive(t1) && !isPrimitive(t2) && glbApprox(t1, t2).isDefined

  /**
   * This returns true iff two variables of type t1 and t2 may share. Two variables share
   * if it is possible to reach the same object by following two chains of fields.
   */
  def mayShare(t1: Type, t2: Type): Boolean = {
    val doShare = sharing.get((t1, t2)) orElse sharing.get((t2, t1))
    if (doShare.isDefined)
      doShare.get
    else {
      val reach1 = reachablesFrom(t1)
      val reach2 = reachablesFrom(t2)
      val sharable = reach1 exists { t1 => reach2 exists { t2 => mayBeAliases(t1, t2) } }
      sharing((t1, t2)) = sharable
      sharing((t2, t1)) = sharable
      sharable
    }
  }
}
