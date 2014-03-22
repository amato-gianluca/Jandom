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

import scala.collection.breakOut
import scala.collection.Set
import it.unich.jandom.utils.DisjointSets

/**
 * The domain for the ALPs domain.
 * @author Gianluca Amato <gamato@unich.it>
 */
class ALPsDomain[OM <: ObjectModel](val om: OM) extends ObjectDomain[OM] {

  import ALPsDomain._
  
  type Span = Map[om.Field, Node]
  
  type EdgeSet = Map[Node, Span]

  def top(types: Seq[om.Type]) = {
    val labels = for { t <- types } yield if (om.mayShare(t, t)) Some(Node()) else None
    val edges: EdgeSet = (for { (t, Some(n)) <- types zip labels } yield { 
      val span: Span = (for { f <- om.fieldsOf(t); tf = om.typeOf(f); if om.mayShare(tf, tf) } yield f -> Node()) (collection.breakOut)
      n -> span
    })  (collection.breakOut)  
    new Property(labels, edges.withDefaultValue(Map.empty), types)
  }

  def bottom(types: Seq[om.Type]) = new Property(Seq.fill(types.size)(None), Map().withDefaultValue(Map.empty), types)

  def apply(labels: Seq[Option[Node]], edges: Map[Node, Map[om.Field, Node]], types: Seq[om.Type]): Property =
    new Property(labels, edges.withDefaultValue(Map.empty), types)

  def apply(labels: Seq[Option[Node]], edges: Seq[(Node, om.Field, Node)], types: Seq[om.Type]): Property = {
    val graphEdges =
      edges.groupBy(_._1).
        mapValues(_.groupBy(_._2).
          mapValues(_.head._3))
    apply(labels, graphEdges, types)
  }

  case class Property(labels: Seq[Option[Node]], edges: EdgeSet, types: Seq[om.Type]) extends ObjectProperty[Property] {

    /**
     * Apply a morphism to a graph.
     */
    private def applyMorphism(m: Morphism) = {
      val newLabels = labels map { _ flatMap m }
      val newEdges = for {
        (src, span) <- edges
        newsrc = m(src)
        if newsrc.isDefined
      } yield newsrc.get -> (for {
        (f, dst) <- span
        newdst = m(dst)
        if newdst.isDefined
      } yield f -> newdst.get)
      new Property(newLabels, newEdges, types)
    }

    /**
     * Returns the type of a node `n`, the least type of all the variables bound to `n`.
     */
    private def nodeType(n: Node): Option[om.Type] = {
      var nodet: Option[om.Type] = None
      for ((Some(`n`), t) <- labels zip types) {
        nodet = if (nodet == None) Some(t) else Some(om.min(nodet.get, t))
      }
      nodet
    }

    /**
     * Returns the new set of edges of node `n` assuming its type becomes t.
     */
    private def expandSpan(n: Node, t: om.Type) = {
      var span = edges(n)
      val nodet = nodeType(n)
      if (nodet.isEmpty || om.lteq(t, nodet.get))
        for (f <- om.fieldsOf(t) -- om.fieldsOf(nodet.get); if !(span isDefinedAt f)) span += f -> Node()
      span
    }

    /**
     *  Returns the set of nodes reachable by `n`.
     */
    private def reachableNodes(n: Node) = {
      val s = collection.mutable.Set(n)
      val q = collection.mutable.Queue(n)
      while (!q.isEmpty) {
        val n = q.dequeue
        for ((f, tgt) <- edges(n); if !(s contains tgt)) {
          s += tgt
          q.enqueue(tgt)
        }
      }
      s
    }

    /**
     * Returns the set of 1st level nodes which may be aliased to a
     * node of type t.
     */
    private def nodesOfType(t: om.Type) = {
      val nodes: Set[Node] = (for {
        (on, nt) <- labels zip types
        n <- on
        if (t == nt || om.lteq(t, nt) || om.lteq(nt, nt))
      } yield n)(collection.breakOut)
      nodes
    }

    /**
     * Returns the set of 1st level nodes which contains the
     * provided field.
     */
    private def nodesWithField(field: om.Field) = {
      val nodes: Set[Node] = (for {
        (on, nt) <- labels zip types
        n <- on
        if om.fieldsOf(nt) contains field
      } yield n)(collection.breakOut)
      nodes
    }

    /**
     * Returns a full span for a node of type t
     */
    private def fullSpan(t: om.Type) = {
      (for (f <- om.fieldsOf(t)) yield f -> Node())(collection.breakOut): Map[om.Field, Node]
    }

    /**
     * Determines whether we may reach a 2^ level node from a
     * given node.
     */
    private def escape(n: Node): Boolean = escape(n, nodeType(n))

    /**
     * Determines whether we may reach a 2^ level node from a
     * given node, assuming we known its nodeType
     */
    private def escape(n: Node, t: Option[om.Type]): Boolean = {
      t match {
        case None => false
        case Some(t) => !(edges(n).keySet intersect om.fieldsOf(t)).isEmpty
      }
    }

    type Domain = ALPsDomain.this.type

    def domain = ALPsDomain.this

    /**
     * Returns the node of the variable i of the graph, None if the variable is null
     */
    def labelOf(i: Int) = labels(i)

    /**
     * Returns the node of the qualified field i.f, None if is null
     */
    def labelOf(i: Int, field: om.Field) = {
      labels(i) flatMap { edges(_).get(field) }
    }

    /**
     * Returns the type of the variable i
     */
    def typeOf(i: Int) = types(i)

    def widening(that: Property): Property = this.union(that)

    def narrowing(that: Property): Property = that

    def union(other: Property): Property = {

      class UnionBuilder {
        private val map1 = collection.mutable.Map[Node, Node]()
        private val map2 = collection.mutable.Map[Node, Node]()
        private val reachable1 = collection.mutable.Set[Node]()
        private val reachable2 = collection.mutable.Set[Node]()
        private val newedges = collection.mutable.Map[Node, Map[om.Field, Node]]().withDefaultValue(Map())

        def getEdges = newedges.toMap.withDefaultValue(Map())

        def copySubgraph(g: Property, reachable: collection.mutable.Set[Node], map: collection.mutable.Map[Node, Node], node: Node): Node = {
          map.get(node) match {
            case None =>
              val newnode = Node()
              reachable += newnode
              map(node) = newnode
              val span = for ((f, n) <- g.edges(node)) yield (f, copySubgraph(g, reachable, map, n))
              newedges(newnode) = span
              newnode
            case Some(newnode) =>
              newnode
          }
        }

        def matchFields(newnode: Node, n1: Node, n2: Node) = {
          reachable1 += newnode
          reachable2 += newnode
          for ((f, n) <- edges(n1)) {
            val newtgt = matchNode(Some(n), other.edges(n2).get(f))
            newedges(newnode) += f -> newtgt
          }
          for ((f, n) <- other.edges(n2)) {
            val newtgt = matchNode(edges(n1).get(f), Some(n))
            newedges(newnode) += f -> newtgt
          }
          newnode
        }

        def matchNode(on1: Option[Node], on2: Option[Node]): Node = {
          (on1, on2) match {
            case (Some(n1), Some(n2)) =>
              (map1 isDefinedAt n1, map2 isDefinedAt n2) match {
                case (false, false) =>
                  map1 += n1 -> n1
                  map2 += n2 -> n1
                  matchFields(n1, n1, n2)
                case (true, false) =>
                  val newnode = Node()
                  map1 += n1 -> newnode
                  matchFields(newnode, n1, n2)
                case (false, true) =>
                  val newnode = Node()
                  map2 += n2 -> newnode
                  matchFields(newnode, n1, n2)
                case (true, true) =>
                  if (map1(n1) == map2(n2))
                    map1(n1)
                  else {
                    val newnode = Node()
                    matchFields(newnode, n1, n2)
                  }
              }
            case (None, Some(n2)) =>
              map2.get(n2) match {
                case None =>
                  newedges(n2) = other.edges(n2)
                  map2(n2) = n2
                  n2
                case Some(n) =>
                  if (reachable1 contains n) {
                    copySubgraph(other, reachable1, collection.mutable.Map(), n2)
                  } else
                    n
              }
            case (Some(n1), None) =>
              map1.get(n1) match {
                case None =>
                  newedges(n1) = edges(n1)
                  map1(n1) = n1
                  n1
                case Some(n) =>
                  if (reachable2 contains n) {
                    copySubgraph(Property.this, reachable2, collection.mutable.Map(), n1)
                  } else
                    n
              }
            case _ =>
              throw new IllegalStateException("This should never happen")
          }
        }
      }

      val unionBuilder = new UnionBuilder()
      val newlabels = for (i <- 0 until labels.length) yield {
        val n1 = labelOf(i)
        val n2 = other.labelOf(i)
        if (n1.isDefined || n2.isDefined)
          Some(unionBuilder.matchNode(labelOf(i), other.labelOf(i)))
        else
          None
      }
      new Property(newlabels, unionBuilder.getEdges, types)
    }

    def intersection(other: Property): Property = {
      val partition = DisjointSets[Option[Node]](None)

      def computePartition(on1: Option[Node], on2: Option[Node]): Unit = {
        if (!partition.inSamePartition(on1, on2)) {
          partition.union(on1, on2)
          (on1, on2) match {
            case (Some(n1), Some(n2)) =>
              for ((f, n) <- edges(n1); if other.nodeType(n2).isDefined) computePartition(Some(n), other.edges(n2).get(f))
              for ((f, n) <- other.edges(n2); if nodeType(n1).isDefined) computePartition(edges(n1).get(f), Some(n))
            case (Some(n1), None) =>
              for ((f, n) <- edges(n1)) computePartition(Some(n), None)
            case (None, Some(n2)) =>
              for ((f, n) <- other.edges(n2)) computePartition(None, Some(n))
            case (None, None) =>
              throw new IllegalStateException("We should never reach this state")
            case (_, _) =>
          }
        }
      }

      def mapWithPartition(n: Option[Node], nullNode: Option[Node]): Option[Node] = {
        val repr = partition(n)
        if (repr == nullNode) None else repr
      }

      for ((on1, on2) <- labels zip other.labels)
        computePartition(on1, on2)
      val nullNode = partition(None)
      val newLabels = labels map { mapWithPartition(_, nullNode) }
      val newEdges = for ((src, span) <- edges; reprsrc <- partition.find(Some(src)); if reprsrc != nullNode)
        yield reprsrc.get -> (for ((f, tgt) <- span; reprtgt = partition(Some(tgt)); if reprtgt != nullNode)
        yield f -> reprtgt.get)
      new Property(newLabels, newEdges.withDefaultValue(Map()), types)

    }

    def isEmpty: Boolean = false

    def mapVariables(rho: Seq[Int]): Property = {
      val newsize = rho.count(_ != -1)
      val revrho = collection.mutable.Buffer.fill(newsize)(0)
      for ((newidx, oldidx) <- rho.zipWithIndex; if newidx != -1) revrho(newidx) = oldidx
      mapVariablesReverse(revrho)
    }

    def mapVariablesReverse(rho: Seq[Int]): Property = {
      new Property(rho map labels, edges, rho map types)
    }

    def top: Property = domain.top(fiber)

    def bottom: Property = domain.bottom(fiber)

    def isTop: Boolean = this == top

    def isBottom: Boolean = labels forall { _.isEmpty }

    def connect(other: Property, common: Int): Property = {
      // we check wether it is possible to reach the non-common variable in this from the common
      // variables. Here sharing might help.
      val escapeFromCommon = (labels zip types) exists { case (Some(n), t) => escape(n, Some(t)); case _ => false }

      if (!escapeFromCommon)
        Property(
          labels.dropRight(common) ++ other.labels.drop(common),
          (edges ++ other.edges) withDefaultValue Map(),
          types.dropRight(common) ++ other.types.drop(common))
      else {
        // set of 1st level nodes reachable in the non-common part of this
        val reachableNodes: Set[Node] = (for (on <- labels; n <- on) yield n)(collection.breakOut)
        val newedges =
          for ((src, span) <- edges) yield src ->
            (if (reachableNodes contains src)
              fullSpan(nodeType(src).get)
            else
              span)
        new Property(
          labels.dropRight(common) ++ other.labels.drop(common),
          (newedges ++ other.edges) withDefaultValue Map(),
          types.dropRight(common) ++ other.types.drop(common))
      }
    }

    def fiber = types

    def dimension = types.size

    def addUnknownVariable(t: om.Type): Property = {
      new Property(labels :+ None, edges, types :+ t)
    }

    def addFreshVariable(t: om.Type): Property = {
      val n = Node()
      val span: Map[om.Field, Node] = om.fieldsOf(t).map { _ -> Node() }(collection.breakOut)
      new Property(labels :+ Some(n), edges updated (n, span), types :+ t)
    }

    def addVariable(t: om.Type): Property = {
      addFreshVariable(t)
    }

    def delVariable(v: Int): Property = {
      if (v == dimension - 1)
        new Property(labels.dropRight(1), edges, types.dropRight(1))
      else
        new Property(labels.take(v) ++ labels.drop(v + 1), edges, types.take(v) ++ types.drop(v + 1))
    }

    def assignNull(dst: Int = dimension - 1): Property = {
      Property(labels.updated(dst, None), edges, types)
    }

    def assignVariable(dst: Int, src: Int): Property = {
      labelOf(src) match {
        case None =>
          assignNull(dst)
        case Some(src) =>
          Property(labels.updated(dst, Some(src)), edges, types)
      }
    }

    def assignVariableToField(dst: Int, field: om.Field, src: Int): Property = {
      (labelOf(dst), labelOf(src)) match {
        case (None, _) =>
          bottom
        case (Some(dstNode), None) =>
          val newspan = edges(dstNode) - field
          Property(labels, edges.updated(dstNode, newspan), types)
        case (Some(dstNode), Some(srcNode)) =>
          val newspan = edges(dstNode) + (field -> srcNode)
          // we need to fresh all node which may possibly be reached by dst.field. In the presence of
          // sharing, we may restrict  possibleAliases using sharing information
          val possibleAliases = nodesWithField(field)
          val newedges = for ((n, span) <- edges) yield if (n == dstNode)
            n -> newspan
          else if (possibleAliases contains n)
            n -> span.updated(field, Node())
          else
            n -> span
          Property(labels, newedges withDefaultValue Map(), types)
      }
    }

    def assignFieldToVariable(dst: Int, src: Int, field: om.Field): Property = {
      if (isDefiniteNull(src))
        bottom
      else {
        labelOf(src, field) match {
          case None => assignNull(dst)
          case Some(src) =>
            Property(labels.updated(dst, Some(src)), edges, types)
        }
      }
    }

    def castVariable(v: Int, newtype: om.Type): Property = {
      labelOf(v) match {
        case None => Property(labels, edges, types updated (v, newtype))
        case Some(n) => Property(labels, edges updated (n, expandSpan(n, newtype)), types updated (v, newtype))
      }
    }

    def isDefiniteNull(v: Int, fieldseq: Seq[om.Field]): Boolean = {
      val loc = labelOf(v)
      (loc, fieldseq) match {
        case (None, _) => true
        case (Some(node), Seq(f)) => edges(node).isDefinedAt(f)
        case _ => false
      }
    }

    def testNull(v: Int): Property = {
      labelOf(v) match {
        case None => this
        case Some(nullnode) =>
          val nodes = reachableNodes(nullnode)
          val newlabels = labels map { _ flatMap { n => if (nodes contains n) None else Some(n) } }
          val newedges = edges mapValues { span => span filterNot { case (f, n) => nodes contains n } }
          /**
           * @todo We need to repeat here the withDefaultValue option.... we probably should consider rewriting
           * stuff so that we do no need them
           */
          new Property(newlabels, newedges withDefaultValue Map(), types)
      }
    }

    def testNotNull(v: Int): Property = {
      if (isDefiniteNull(v))
        bottom
      else
        this
    }

    /**
     * Returns, if it exists, a morphism connecting `this` and `other`.
     * @param other the other graph. We assume the two graphs are over the same fiber, since
     * we do not check this property.
     * @returns `None` if the two graphs are incomparable, otherwise `Some(i,m)` where `i`
     * is the same result of `tryCompare` and `m` is the morphism, either `this |-> other`
     * or `other |-> this` according to the value of `i`.
     */
    def tryMorphism(other: Property): Option[(Int, Morphism)] = {

      class MorphismBuilder {
        private var status: Option[Int] = Some(0)
        private var map1 = Map[Node, Option[Node]]()
        private var map2 = Map[Node, Option[Node]]()

        def direction = this.status

        def morphism: Option[Morphism] = status match {
          case None => None
          case Some(-1) => Some(map2.withDefaultValue(None))
          case _ => Some(map1.withDefaultValue(None))
        }

        def matchFields(n1: Node, n2: Node) = {
          if (nodeType(n1).isDefined && other.nodeType(n2).isDefined) {
            for ((f, n) <- edges(n1)) matchNode(Some(n), other.edges(n2).get(f))
            for ((f, n) <- other.edges(n2)) matchNode(edges(n1).get(f), Some(n))
          }
        }

        def matchNode(on1: Option[Node], on2: Option[Node]): Option[Int] = {
          (status, on1, on2) match {
            case (Some(0), Some(n1), Some(n2)) =>
              (map1 isDefinedAt n1, map2 isDefinedAt n2) match {
                case (false, false) =>
                  map1 += n1 -> on2
                  map2 += n2 -> on1
                  matchFields(n1, n2)
                case (true, false) =>
                  map2 += n2 -> on1
                  status = Some(-1)
                  matchFields(n1, n2)
                case (false, true) =>
                  map1 += n1 -> on2
                  status = Some(1)
                  matchFields(n1, n2)
                case (true, true) =>
                  status = if (map1(n1) == on2) status else None
              }
            case (Some(-1), Some(n1), Some(n2)) =>
              if (!(map2 isDefinedAt n2)) {
                map2 += n2 -> on1
                matchFields(n1, n2)
              } else
                status = if (map2(n2) != on1) None else status
            case (Some(1), Some(n1), Some(n2)) =>
              if (!(map1 isDefinedAt n1)) {
                map1 += n1 -> on2
                matchFields(n1, n2)
              } else
                status = if (map1(n1) != on2) None else status
            case (Some(1), None, Some(n2)) =>
              status = None
            case (Some(-1), Some(n1), None) =>
              status = None
            case (_, None, Some(n2)) =>
              if (!(map2 isDefinedAt n2)) {
                status = Some(-1)
                map2 += n2 -> None
              } else
                status = if (map2(n2) == None) status else None
            case (_, Some(n1), None) =>
              if (!(map1 isDefinedAt n1)) {
                status = Some(1)
                map1 += n1 -> None
              } else
                status = if (map1(n1) == None) status else None
            case _ =>
          }
          status
        }
      }

      val morphismBuilder = new MorphismBuilder()
      for (i <- 0 until labels.length)
        morphismBuilder.matchNode(labelOf(i), other.labelOf(i))
      morphismBuilder.direction match {
        case None => None
        case Some(d) => Some((d, morphismBuilder.morphism.get))
      }
    }

    def tryCompareTo[B >: Property](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] = {
      other match {
        case other: Property =>
          // this check might be removed to improve performance
          if (fiber != other.fiber)
            None
          else
            tryMorphism(other) map { _._1 }
        case _ => None
      }
    }

    def mkString(vars: Seq[String]) = mkString(vars, false)

    def mkString(vars: Seq[String], hashValued: Boolean) = {
      val nodenames = collection.mutable.Map[Node, Int]()
      var currnum = -1
      val s1 = for { (Some(n), v) <- labels.zipWithIndex } yield {
        val l = if (hashValued) n.hashCode() else nodenames.getOrElseUpdate(n, { currnum += 1; currnum })
        s"${vars(v)} : ${typeOf(v)} -> n${l}"
      }
      val s2 = for { Some(n) <- labels.toSet; (f, ntgt) <- edges(n) } yield {
        val l1 = if (hashValued) n.hashCode() else nodenames(n)
        val l2 = if (hashValued) ntgt.hashCode() else nodenames.getOrElseUpdate(ntgt, { currnum += 1; currnum })
        s"n${l1} -- ${f} --> n${l2}"
      }
      s1.mkString("Vars: ", ", ", "") + " // " + s2.mkString("Edges: ", ", ", "")
    }
  }
}

object ALPsDomain extends ObjectDomainFactory {
  
  /**
   * A node in the ALPs graph. We tried to use integer for nodes, but since they
   * are mostly put inside maps, hence they should be boxed, it is not very
   * convenient.
   */
  class Node private {
    override def toString = s"n${this.hashCode}"
  }

  /**
   * The factory for nodes.
   */
  object Node {    
    def apply(): Node = new Node      
  }
  
  /**
   * A morphism is a function from Node to Option[Node]. We could use 
   * partial function, but I think a standard function is more convenient.
   */
  type Morphism = Function[Node, Option[Node]]
  
  def apply[OM <: ObjectModel](om: OM) = new ALPsDomain(om)
}
