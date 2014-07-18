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
 * A location domain is an abstraction of locations is a program.
 * @author Gianluca Amato <gamato@unich.it>
 */
trait LocationDomain[OM <: ObjectModel, Node] {
  val om: OM   
 
  type Property <: LocationProperty[Property]    
  
  def bottom(nodes: Iterable[Node]): Property
  
  def top(node: Iterable[Node]): Property
  
  def top(node: Iterable[(Node, om.Type)], withType: Boolean): Property

  trait LocationProperty[Property <: LocationProperty[Property]]  {
    def applyMorphism(m: Node => Option[Node]): Property
    def applyMorphism(m: collection.Map[Node,Node]): Property
    def union(that: Property): Property
    def addFreshNodes(nodes: Iterable[Node]): Property
    def addChildren(parent: Node, children: Iterable[Node]): Property
    def delChildren(parent: Node, children: Set[Node]): Property
    def delNodes(nodes: collection.Set[Node]): Property
    /**
     * Only keeps nodes which satisy `p`
     */
    def filterNodes(p: Node => Boolean): Property 
    def mkString(nodeNames: Node => String): String
    def mayShare(n1: Node, n2: Node): Boolean
    def mayBeAliases(n1: Node, n2: Node): Boolean
  } 
}
