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

import scala.annotation.tailrec
import it.unich.jandom.utils.DisjointSets

/**
 * The implementation of the ALPs domain. This mixes aliasing, pair sharing and linearity. At
 * the moment, linearity is not yet implemented.
 * @param om the object model to use for the domain
 * @author Gianluca Amato <gamato@unich.it>
 */

class ALPsDomain[+OM <: ObjectModel](val om: OM) extends ObjectDomain[OM] {

  import AliasingDomain.Node
  import aldom.EdgeSet
  import aldom.Span

  /**
   * The aliasing domain used to manipulate the aliasing graph component
   */
  private[ALPsDomain] val aldom = new AliasingDomain[om.type](om)

  /**
   * The ALPs domain directly implements aliasing and depends on `plug-ins` for all other
   * properties. At the moment, `psdom` is the plug-in for pair-sharing and cannot be changed.
   */
  private[ALPsDomain] val psdom = new PairSharing[om.type, Node](om)

  def top(types: Fiber) = {
    val g = aldom.top(types)
    Property(g, psdom.top(g.nodes))
  }

  def bottom(types: Fiber) = {
    val g = aldom.bottom(types)
    Property(g, psdom.bottom(g.nodes)) // could be optimized, since we know that g.nodes is empty
  }

  /**
   * Build an ALPs graph with given labels, edges, types, and pair sharing information.
   */
  def apply(labels: Seq[Option[Node]], edges: EdgeSet, types: Seq[om.Type], ps: psdom.Property): Property =
    Property(aldom(labels, edges, types), ps)

  /**
   * Build an ALPs graph with given labels, edges, types, and pair sharing information. Edges are given as a sequence
   * of triples `(n1,f,n2)` where `n1` is the source node, `n2` is the target node and `f` the field name.
   */
  def apply(labels: Seq[Option[Node]], edges: Seq[(Node, om.Field, Node)], types: Seq[om.Type], ps: psdom.Property): Property =
    Property(aldom(labels, edges, types), ps)

  /**
   * Build an ALPs graph with given labels, edges and types. Pair sharing information is set to the top value,
   * i.e., every pairs of variables which may share by typing information is set to possibly share.
   */
  def apply(labels: Seq[Option[Node]], edges: EdgeSet, types: Seq[om.Type]): Property = {
    val g = aldom(labels, edges, types)
    val typednodes = g.nodes map { (n: Node) => (n, g.nodeType(n)) }
    new Property(g, psdom.top(typednodes))
  }

  /**
   * Build an ALPs graph with given labels, edges, types, and pair sharing information. Edges are given as a sequence
   * of triples `(n1,f,n2)` where `n1` is the source node, `n2` is the target node and `f` the field name.
   * Pair sharing information is set to the top value,
   * i.e., every pairs of variables which may share by typing information is set to possibly share
   */
  def apply(labels: Seq[Option[Node]], edges: Seq[(Node, om.Field, Node)], types: Seq[om.Type]): Property = {
    val g = aldom(labels, edges, types)
    val typednodes = g.nodes map { (n: Node) => (n, g.nodeType(n)) }
    new Property(g, psdom.top(typednodes))
  }

  /**
   * The class implements ALPs graph
   * @param g the underlying aliasing graph
   * @param ps pair sharing information
   */
  case class Property(val g: aldom.Property, val ps: psdom.Property) extends ObjectProperty[Property] {

    /**
     * For compatibility, this builds an aliasing graph from its components: labels, edges, types and
     * pair sharing information.
     */
    def this(labels: Seq[Option[Node]], edges: EdgeSet, types: Seq[om.Type], ps: psdom.Property) =
      this(aldom.Property(labels, edges, types), ps)

    type Domain = ALPsDomain.this.type

    def labels = g.labels // it should be private, but I have problem with tests

    def edges = g.edges

    def types = g.types

    // BROKEN (we should consider pair sharing information)
    def tryCompareTo[B >: Property](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] = {
      other match {
        case other: Property =>
          assume(fiber == other.fiber)
          g.tryMorphism(other.g) map { _._1 }
        case _ => None
      }
    }

    def domain = ALPsDomain.this

    def widening(that: Property): Property = this.union(that)

    def narrowing(that: Property): Property = that

    def union(other: Property): Property = {
      val (newGraph, m1, m2) = g unionWithMorphisms other.g
      val newPs = ps.applyMorphism(m1) union other.ps.applyMorphism(m2)
      new Property(newGraph, ps)
    }

    // BROKEN (we should better consider pair sharing information)
    def intersection(other: Property): Property = {
      val (newGraph, m1, m2) = g intersectionWithMorphisms other.g
      val newPs = ps.applyMorphism(m1) intersect other.ps.applyMorphism(m2)
      new Property(newGraph, newPs)
    }

    def isEmpty: Boolean = false

    def fiber = g.types
    
    def dimension = g.dimension
    
    def addUnknownVariable(t: om.Type): Property = {
      new Property(g.addUnknownVariable(t), ps)
    }

    def addFreshVariable(t: om.Type): Property = {
      val (newg, n, span) = g.addFreshVariableWithSpan(t)      
      val freshps = psdom.top(Iterable(n) ++ span.values)
      new Property(newg, ps union freshps)
    }

    def addVariable(t: om.Type): Property = {
      addFreshVariable(t)
    }    
    
    def delVariable(v: Int): Property = {
      val (newEdges, newPs) = labels(v) match {
        case None => (edges, ps)
        case Some(n) =>
          val reachable = g.reachableNodesForbidden(n)
          if (reachable contains n) {
            val newNodeTypes = for ((Some(`n`), i) <- labels.zipWithIndex) yield types(i)
            val newType = om.glbApprox(newNodeTypes).get
            val (newSpan, detached) = g.reduceSpan(n, newType)
            val removeFromPs = detached filterNot { reachable contains _ }
            val newPs = ps.delChildren(n, removeFromPs.toSet)
            (edges updated (n, newSpan), newPs)
          } else {
            (edges filterKeys { reachable contains _ }, ps filterNodes { reachable contains _ })
          }
      }
      if (v == dimension - 1)
        new Property(labels.dropRight(1), newEdges withDefaultValue Map(), types.dropRight(1), newPs)
      else
        new Property(labels.take(v) ++ labels.drop(v + 1), newEdges withDefaultValue Map(), types.take(v) ++ types.drop(v + 1), newPs)
      // TODO: perhaps I should remove all unreachable nodes
    }
    
    // BROKEN
    def mapVariables(rho: Seq[Int]): Property = Property(g.mapVariables(rho), ps)       
    
    def top: Property = domain.top(fiber)

    def bottom: Property = domain.bottom(fiber)

    def isTop: Boolean = this == top

    def isBottom: Boolean = labels forall { _.isEmpty }

    def connect(other: Property, common: Int): Property = {
      // we check whether it is possible to reach the non-common variable in this from the common
      // variables. Here sharing might help.
      val escapeFromCommon = (labels zip types) exists { case (Some(n), t) => g.escape(n, t); case _ => false }
      if (!escapeFromCommon) {
        val nodes = g.reachableNodesFrom(labels.takeRight(common).flatten: _*)
        new Property(
          labels.dropRight(common) ++ other.labels.drop(common),
          (edges ++ other.edges) withDefaultValue Map(),
          types.dropRight(common) ++ other.types.drop(common), ps.delNodes(nodes))
      } else {
        // set of 1st level nodes reachable in the non-common part of this
        val reachableNodes: Set[Node] = (for (on <- labels; n <- on) yield n)(collection.breakOut)
        val newnodes = collection.mutable.Buffer[Node]()
        val newedges =
          for ((src, span) <- edges) yield src ->
            (if (reachableNodes contains src) {
              val fs = g.fullSpan(g.nodeType(src).get)
              newnodes ++= fs.values
              fs
            } else
              span)
        new Property(
          labels.dropRight(common) ++ other.labels.drop(common),
          (newedges ++ other.edges) withDefaultValue Map(),
          types.dropRight(common) ++ other.types.drop(common), ps addFreshNodes newnodes)
      }
    }
        
    def mkString(vars: Seq[String]) = {
      val nodeNames = { (n: Node) => "n" + n }
      g.mkString(vars) + " // " + ps.mkString(nodeNames)
    }
    
    // BROKEN
    def assignNull(dst: Int = dimension - 1): Property = {
      Property(g.assignNull(dst), ps)
    }
    
    // BROKEN
    def assignVariable(dst: Int, src: Int): Property = {
      Property(g.assignVariable(dst,src), ps)      
    }

    def assignVariableToField(dst: Int, field: om.Field, src: Int): Property = {
       Property(g.assignVariableToField(dst, field, src), ps)
    }     

    def assignFieldToVariable(dst: Int, src: Int, field: om.Field): Property = {
       Property(g.assignFieldToVariable(dst, src, field), ps)
    }      

    // Here I am assuming objects are only cast downwards
    def castVariable(v: Int, newtype: om.Type): Property = {
      assume(om.lteq(newtype, types(v)))
      val (newgraph, n, newspan) = g.castVariableWithSpan(v, newtype)      
      n match {
        case None => 
          Property(newgraph, ps)
        case Some(n) =>
          Property(newgraph, ps.addChildren(n, newspan.values))
      }
    }

    def testNull(v: Int): Property = {
      val (newgraph, nodes) = g.testNullWithNodes(v)
      new Property(newgraph,  ps delNodes nodes)   
    }

    def testNotNull(v: Int): Property = {
      if (mustBeNull(v))
        bottom
      else
        this
    }

    def mustBeNull(v: Int, fs: Iterable[om.Field]) = g.nodeOf(v, fs).isEmpty

    def mayBeNull(v: Int, fs: Iterable[om.Field]) = true

    def mayShare(v1: Int, fs1: Iterable[om.Field], v2: Int, fs2: Iterable[om.Field]) = (g.nodeOf(v1, fs1), g.nodeOf(v2, fs2)) match {
      case (Some(n1), Some(n2)) =>
        val nodes1 = g.reachableNodesFrom(n1)
        val nodes2 = g.reachableNodesFrom(n2)
        nodes1 exists { n => nodes2 exists { m => ps.mayShare(n, m) } }
      case _ => false
    }

    def mustShare(v1: Int, fs1: Iterable[om.Field], v2: Int, fs2: Iterable[om.Field]) = false

    def mayBeAliases(v1: Int, v2: Int) = (labels(v1), labels(v2)) match {
      case (Some(n1), Some(n2)) => om.mayBeAliases(types(v1), types(v2)) && ps.mayBeAliases(n1, n2)
      case _ => false
    }

    def mustBeAliases(v1: Int, v2: Int) = labels(v1).isDefined && labels(v2) == labels(v1)

    def mayBeWeakAliases(v1: Int, v2: Int) = true

    def mustBeWeakAliases(v1: Int, v2: Int) = labels(v1) == labels(v2)

    def typeOf(v: Int, fs: Iterable[om.Field]): Option[om.Type] = {
      g.typeOf(v, fs)
    }

  }
}

object ALPsDomain extends ObjectDomainFactory {
  def apply[OM <: ObjectModel](om: OM) = new ALPsDomain(om)
}
