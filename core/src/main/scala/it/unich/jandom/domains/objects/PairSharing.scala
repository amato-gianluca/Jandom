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

import it.unich.jandom.objectmodels.ObjectModel

/**
 * @author Gianluca Amato <gamato@unich.it>
 */

class PairSharing[OM <: ObjectModel, Node](val om: OM) extends LocationDomain[OM, Node]() {

  implicit object nodeOrdering extends Ordering[Node] {
    def compare(x: Node, y: Node) = x.hashCode - y.hashCode
  }

  private def allPsPairs(nodetypes: Iterable[(Node, om.Type)]) = {
    for ((n1, t1) <- nodetypes; (n2, t2) <- nodetypes; if om.mayShare(t1, t2)) yield UP(n1, n2)
  }
  
  def top(nodes: Iterable[Node]): Property = {
    val ps: Set[UP[Node]] = (for (n1 <- nodes; n2 <- nodes) yield UP(n1, n2))(collection.breakOut)
    Property(ps)
  }
  
  def top(nodetypes: Iterable[(Node, om.Type)], withType: Boolean = true) = Property(allPsPairs(nodetypes).toSet)

  def bottom(nodes: Iterable[Node]): Property = Property( (for (n <- nodes) yield UP(n,n)).toSet )
  
  def apply(ps: Iterable[UP[Node]]) = new Property(ps.toSet) 

  case class Property(ps: Set[UP[Node]]) extends LocationProperty[Property] {

    implicit object nodeOrdering extends Ordering[Node] {
      def compare(x: Node, y: Node) = x.hashCode - y.hashCode
    }

    def mkString(nodeNames: Node => String) = {
      val strings = for (UP(n1,n2) <- ps) yield s"(${nodeNames(n1)},${nodeNames(n2)})"
      strings.mkString("{ ", ", ", " }")
    }
    
    def applyMorphism(m: Node => Option[Node]) = {
      Property(for (UP(n1, n2) <- ps; m1new = m(n1); if m1new.isDefined; m2new = m(n2); if m2new.isDefined)
        yield UP(m1new.get, m2new.get))
    }

    def applyMorphism(m: collection.Map[Node, Node]) = {
      Property(for (UP(n1, n2) <- ps; m1new = m.get(n1); if m1new.isDefined; m2new = m.get(n2); if m2new.isDefined)
        yield UP(m1new.get, m2new.get))
    }

    def union(that: Property) = new Property(ps ++ that.ps)
    
    def intersect(that: Property) = new Property(ps intersect that.ps)

    def addFreshNodes(nodes: Iterable[Node]) = {
      val newPairs = nodes map { n => UP(n, n) }
      new Property(ps ++ newPairs)
    }

    def addChildren(parent: Node, children: Iterable[Node]) = {
      val parentRelated = ps filter { _ contains parent }
      val newPs = (children flatMap { (child) => parentRelated map { _.replace(parent, child) } })
      new Property(ps ++ newPs)
    }

    def delChildren(parent: Node, children: Set[Node]) = {
      val (deleting, remaining) = ps partition { case UP(n1, n2) => (children contains n1) || (children contains n2) }
      val newPs = children map { (child) => deleting map { _.replace(child, parent) } }
      new Property(newPs.fold(remaining) { _ ++ _ })
    }

    def delNodes(nodes: collection.Set[Node]) = {
      new Property(ps filterNot { case UP(n1, n2) => (nodes contains n1) || (nodes contains n2) })
    }
    
    def filterNodes(f: Node => Boolean) = {
      new Property(ps filter { case UP(n1, n2) => f(n1) && f(n2) })
    }

    def mayShare(n1: Node, n2: Node) = ps contains UP(n1, n2)

    def mayBeAliases(n1: Node, n2: Node) = mayShare(n1, n2)
  }
}
