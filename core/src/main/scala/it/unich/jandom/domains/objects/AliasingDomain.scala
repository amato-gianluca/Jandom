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

import it.unich.jandom.utils.DisjointSets
import scala.annotation.tailrec

/**
 * The domain for definite weak aliasing. Two identifiers are weak aliased if either they are
 * both null, or they point to the same location. This information is encoded in a graph called
 * aliasing graph.
 * @note this class mix the standard object domain interface with low level functionalities which should be factored out.
 * @author Gianluca Amato <gamato@unich.it>
 */
class AliasingDomain[OM <: ObjectModel](val om: OM) extends ObjectDomain[OM] {

  import AliasingDomain._

  /**
   * A Span is a set of edges departing from a node and labeled by field names
   */
  type Span = Map[om.Field, Node]

  /**
   * An EdgeSet is a map from nodes in a graph to its corresponding span
   */
  type EdgeSet = Map[Node, Span]

  def top(types: Fiber) = {
    val labels = for { t <- types } yield if (om.mayShare(t, t)) Some(Node()) else None
    val edges: EdgeSet = (for { (t, Some(n)) <- types zip labels } yield {
      val span: Span = (for { f <- om.fieldsOf(t); tf = om.typeOf(f); if om.mayShare(tf, tf) } yield f -> Node())(collection.breakOut)
      n -> span
    })(collection.breakOut)
    Property(labels, edges.withDefaultValue(Map.empty), types)
  }

  def bottom(types: Fiber) =
    Property(Seq.fill(types.size)(None), Map().withDefaultValue(Map.empty), types)

  /**
   * Builds an aliasing graph with given labels, edges and, types. Edges are given in the form
   * of an EdgeSet.
   */
  def apply(labels: Seq[Option[Node]], edges: Map[Node, Map[om.Field, Node]], types: Seq[om.Type]): Property =
    new Property(labels, edges.withDefaultValue(Map.empty), types)

  /**
   * Builds an aliasing graph with given labels, edges and types. Edges are given as a sequence
   * of triples `(n1,f,n2)` where `n1` is the source node, `n2` is the target node and `f` the field name.
   */
  def apply(labels: Seq[Option[Node]], edges: Seq[(Node, om.Field, Node)], types: Seq[om.Type]): Property = {
    val graphEdges =
      edges.groupBy(_._1).
        mapValues(_.groupBy(_._2).
          mapValues(_.head._3))
    apply(labels, graphEdges, types)
  }

  /**
   * Aliasing information is represented with an aliasing graph.
   * @param labels sequence of nodes associated to each variable. A value of `None` denotes a
   * definitively null variable.
   * @param edges set of edges of the graph. The set of edges should be complete, i.e. it
   * should not exists a node `n` in a graph suces that `edges(n)` is not defined. This could
   * be accomplished with maps with default values, if needed.
   * @param types sequence of declared types of the variables.
   */
  case class Property(labels: Seq[Option[Node]], edges: EdgeSet, types: Seq[om.Type]) extends ObjectProperty[Property] {

    /**
     * Returns the set of nodes reachable by elements in `nodes`.
     */
    private[objects] def reachableNodesFrom(nodes: Node*): collection.Set[Node] = {
      val s = collection.mutable.Set(nodes: _*)
      val q = collection.mutable.Queue(nodes: _*)
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
     * Returns the set of nodes reachable from the set of root nodes
     * minus `n`.
     * @note This is more efficient that just resorting to reachableNodesFrom
     */
    private[objects] def reachableNodesForbidden(forbidden: Node): collection.Set[Node] = {
      assume(isFirstLevel(forbidden))
      val firstLevel = collection.mutable.Set[Node]()
      for (Some(n) <- labels; if n != forbidden) firstLevel += n
      val secondLevel = firstLevel flatMap { edges(_).values }
      firstLevel ++= secondLevel
    }

    /**
     * Returns the set of all the nodes in the graph.
     * @note This is more efficient that just resorting to reachableNodesFrom
     */
    private[objects] def nodes: collection.Set[Node] = {
      val allNodes = collection.mutable.Set[Node]()
      for (Some(n) <- labels) allNodes += n
      for ((src, span) <- edges; (f, n) <- span) allNodes += n
      allNodes
    }

    /**
     * Determines whether we may reach a 2^ level node from a
     * given 1^ level node.
     */
    private[objects] def escape(n: Node): Boolean = {
      assume(isFirstLevel(n))
      escape(n, nodeType(n).get)
    }

    /**
     * Determines whether we may reach a 2^ level node from a
     * given given 1^ level node, assuming we known its nodeType.
     */
    private[objects] def escape(n: Node, t: om.Type): Boolean = {
      assume(isFirstLevel(n))
      !(edges(n).keySet intersect om.fieldsOf(t)).isEmpty
    }

    /**
     * Determines whether the node `n` is a 1^ level (root) node
     */
    private[objects] def isFirstLevel(n: Node): Boolean = {
      labels contains Some(n)
    }

    /**
     * Returns the set of root nodes which may be aliased to a
     * node of type t.
     */
    private[objects] def nodesOfType(t: om.Type): Set[Node] = {
      val nodes: Set[Node] = (for {
        (on, nt) <- labels zip types
        n <- on
        if (t == nt || om.lteq(t, nt) || om.lteq(nt, t))
      } yield n)(collection.breakOut)
      nodes
    }

    /**
     * Returns the set of 1st level nodes which contains the
     * provided field.
     */
    private[objects] def nodesWithField(field: om.Field) = {
      val nodes: Set[Node] = (for {
        (on, nt) <- labels zip types
        n <- on
        if om.fieldsOf(nt) contains field
      } yield n)(collection.breakOut)
      nodes
    }

    /**
     * Returns the node associated with a length one reachable identifier, or `None` otherwise
     */
    private[objects] def nodeOf(v: Int, f: om.Field): Option[Node] = {
      labels(v) flatMap { edges(_).get(f) }
    }

    /**
     * Returns the node associated to a field, or `None` otherwise
     */
    private[objects] def nodeOf(v: Int, fs: Iterable[om.Field]): Option[Node] = {
      val on = labels(v)
      if (on.isEmpty) None else nodeOf(on.get, fs)
    }

    /**
     * Follow the chain of fields starting from node `n` and returns the nodes we reach,
     * or `None` if some fields is not defined in the graph.
     */
    @tailrec
    private def nodeOf(n: Node, fs: Iterable[om.Field]): Option[Node] = {
      if (fs.isEmpty)
        Some(n)
      else edges(n).get(fs.head) match {
        case None => None
        case Some(nnew) => nodeOf(nnew, fs.tail)
      }
    }

    /**
     * Returns the glb approximation of the 1st level types for the node n
     */
    private[objects] def nodeType(n: Node): Option[om.Type] = {
      om.glbApprox(for ((Some(`n`), t) <- labels zip types) yield t)
    }

    /**
     * Returns a glb approximation of all types pointing to node n
     */
    private[objects] def completeNodeType(n: Node): Option[om.Type] = {
      val firstLevels = for ((Some(`n`), t) <- labels zip types) yield t
      val secondLevels = for ((_, span) <- edges; (f, `n`) <- span) yield om.typeOf(f)
      om.glbApprox(firstLevels ++ secondLevels)
    }

    /**
     * Returns the new set of edges of node `n` assuming its type becomes `t`. If `t`
     * is not a subtype of the current type of `n`, it does nothing.
     */
    private[objects] def expandSpan(n: Node, t: om.Type): Span = {
      var span = edges(n)
      val nodet = nodeType(n).get
      if (om.lteq(t, nodet))
        for (f <- om.fieldsOf(t) -- om.fieldsOf(nodet); if !(span isDefinedAt f)) span += f -> Node()
      span
    }

    /**
     * Returns the edges removed from node `n` when its type becomes `t`. We assume that
     * `t` is a supertype of the type of `n`
     */
    private[objects] def reduceSpan(n: Node, t: om.Type): (Span, Iterable[Node]) = {
      assume(om.lteq(nodeType(n).get, t))
      val removedFields = om.fieldsOf(nodeType(n).get) -- om.fieldsOf(t)
      val removedNodes = for { (f, dst) <- edges(n); if removedFields contains f } yield dst
      (edges(n) -- removedFields, removedNodes)
    }

    /**
     * Returns a full span for a node of type t
     */
    private[objects] def fullSpan(t: om.Type): Span = {
      (for (f <- om.fieldsOf(t)) yield f -> Node())(collection.breakOut)
    }

    /**
     * Returns, if it exists, a morphism connecting `this` and `other`.
     * @param other the other graph. We assume the two graphs are over the same fiber, since
     * we do not check this property.
     * @returns `None` if the two graphs are incomparable, otherwise `Some(i,m)` where `i`
     * is the same result of `tryCompare` and `m` is the morphism, either `this |-> other`
     * or `other |-> this` according to the value of `i`.
     */
    private[objects] def tryMorphism(other: Property): Option[(Int, Morphism)] = {

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
          if (isFirstLevel(n1) && other.isFirstLevel(n2)) {
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

    /**
     * Apply a morphism to a graph.
     */
    private[objects] def applyMorphism(m: Morphism): Property = {
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
      Property(newLabels, newEdges, types)
    }

    type Domain = AliasingDomain.this.type

    def domain = AliasingDomain.this

    def typeOf(v: Int, fs: Iterable[om.Field]) = nodeOf(v, fs) flatMap nodeType

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

    def widening(that: Property): Property = this.union(that)

    def narrowing(that: Property): Property = that

    private[objects] def unionWithMorphisms(other: Property) = {

      class UnionBuilder {
        val map1 = collection.mutable.Map[Node, Node]()
        val map2 = collection.mutable.Map[Node, Node]()
        val newedges = collection.mutable.Map[Node, Map[om.Field, Node]]().withDefaultValue(Map())
        private val reachable1 = collection.mutable.Set[Node]()
        private val reachable2 = collection.mutable.Set[Node]()

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

        def matchFields(newnode: Node, n1: Node, n2: Node): Node = {
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
      val unionGraph = new Property(newlabels, unionBuilder.newedges.toMap.withDefaultValue(Map.empty), types)
      (unionGraph, unionBuilder.map1, unionBuilder.map2)
    }

    def union(other: Property): Property = (this unionWithMorphisms other)._1

    private[objects] def intersectionWithMorphisms(other: Property): (Property, Morphism, Morphism) = {
      val partition = DisjointSets[Option[Node]](None)

      def computePartition(on1: Option[Node], on2: Option[Node]): Unit = {
        if (!partition.inSamePartition(on1, on2)) {
          partition.union(on1, on2)
          (on1, on2) match {
            case (Some(n1), Some(n2)) =>
              for ((f, n) <- edges(n1); if other.isFirstLevel(n2)) computePartition(Some(n), other.edges(n2).get(f))
              for ((f, n) <- other.edges(n2); if isFirstLevel(n1)) computePartition(edges(n1).get(f), Some(n))
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

      def partitionToMap(nullNode: Option[Node]): Morphism = { (n: Node) =>
        val repr = partition(Some(n))
        if (repr == nullNode) None else repr
      }

      for ((on1, on2) <- labels zip other.labels)
        computePartition(on1, on2)

      val nullNode = partition(None)
      val newLabels = labels map { mapWithPartition(_, nullNode) }
      val newEdges = for ((src, span) <- edges; reprsrc <- partition.find(Some(src)); if reprsrc != nullNode)
        yield reprsrc.get -> (for ((f, tgt) <- span; reprtgt = partition(Some(tgt)); if reprtgt != nullNode)
        yield f -> reprtgt.get)
      val newGraph = new Property(newLabels, newEdges.withDefaultValue(Map.empty), types)
      val morphism = partitionToMap(nullNode)
      (newGraph, morphism, morphism)
    }

    def intersection(other: Property): Property = (this intersectionWithMorphisms other)._1

    def isEmpty: Boolean = false

    def fiber = types

    def dimension = types.length

    def addUnknownVariable(t: om.Type): Property = {
      new Property(labels :+ None, edges, types :+ t)
    }

    /**
     * Returns the aliasing graph obtained by adding a fresh variable, together
     * with the node corresponding to its variable and its fresh span.
     */
    private[objects] def addFreshVariableWithSpan(t: om.Type): (Property, Node, Span) = {
      val n = Node()
      val span: Span = om.fieldsOf(t).map { _ -> Node() }(collection.breakOut)
      val newgraph = new Property(labels :+ Some(n), edges updated (n, span), types :+ t)
      (newgraph, n, span)
    }

    def addFreshVariable(t: om.Type): Property = addFreshVariableWithSpan(t)._1

    def addVariable(t: om.Type): Property = {
      addFreshVariable(t)
    }

    def delVariable(v: Int): Property = {
      if (v == dimension - 1)
        new Property(labels.dropRight(1), edges, types.dropRight(1))
      else
        new Property(labels.take(v) ++ labels.drop(v + 1), edges, types.take(v) ++ types.drop(v + 1))
    }

    def mapVariables(rho: Seq[Int]): Property = {
      val newsize = rho.count(_ != -1)
      val revrho = collection.mutable.Buffer.fill(newsize)(0)
      for ((newidx, oldidx) <- rho.zipWithIndex; if newidx != -1) revrho(newidx) = oldidx
      mapVariablesReverse(revrho)
    }

    private def mapVariablesReverse(rho: Seq[Int]): Property = {
      new Property(rho map labels, edges, rho map types)
    }

    def top: Property = domain.top(fiber)

    def bottom: Property = domain.bottom(fiber)

    def isTop: Boolean = this == top

    def isBottom: Boolean = labels forall { _.isEmpty }

    def connect(other: Property, common: Int): Property = {
      // we check wether it is possible to reach the non-common variable in this from the common
      // variables. Here sharing might help.
      val escapeFromCommon = (labels zip types) exists { case (Some(n), t) => escape(n, t); case _ => false }

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

    def mkString(vars: Seq[String]) = {
      val s1 = for { (Some(n), v) <- labels.zipWithIndex } yield {
        s"${vars(v)} : ${types(v)} -> n${n}"
      }
      val s2 = for { Some(n) <- labels.toSet; (f, ntgt) <- edges(n) } yield {
        s"n${n} -- ${f} --> n${ntgt}"
      }
      s1.mkString("Vars: ", ", ", "") + " // " + s2.mkString("Edges: ", ", ", "")
    }

    def assignNull(dst: Int = dimension - 1): Property = {
      Property(labels.updated(dst, None), edges, types)
      // TODO: perhaps I should remove all unreachable nodes
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
          val newedges = for ((n, span) <- edges.updated(dstNode, newspan)) yield if (n == dstNode)
            n -> span
          else if (possibleAliases contains n)
            n -> span.updated(field, Node())
          else
            n -> span
          Property(labels, newedges withDefaultValue Map(), types)
      }
    }

    def assignFieldToVariable(dst: Int, src: Int, field: om.Field): Property = {
      if (mustBeNull(src))
        bottom
      else {
        labelOf(src, field) match {
          case None => assignNull(dst)
          case Some(src) =>
            Property(labels.updated(dst, Some(src)), edges, types)
        }
      }
    }

    /**
     *  Return the result of a cast and the new span added to the variable v and its node
     */
    private[objects] def castVariableWithSpan(v: Int, newtype: om.Type): (Property, Option[Node], Span) = {
      assume(om.lteq(newtype, types(v)))
      labelOf(v) match {
        case None =>
          val newgraph = Property(labels, edges, types updated (v, newtype))
          (newgraph, None, Map.empty)
        case Some(n) =>
          val newspan = expandSpan(n, newtype)
          val newgraph = Property(labels, edges updated (n, newspan), types updated (v, newtype))
          (newgraph, Some(n), newspan)
      }
    }

    def castVariable(v: Int, newtype: om.Type): Property = castVariableWithSpan(v, newtype)._1

    /**
     * Returns the result of a nullness test and the nodes reachable from the checked variable
     */
    private[objects] def testNullWithNodes(v: Int): (Property, collection.Set[Node]) = {
      labelOf(v) match {
        case None => (this, Set.empty)
        case Some(nullnode) =>
          val nodes = reachableNodesFrom(nullnode)
          val newlabels = labels map { _ flatMap { n => if (nodes contains n) None else Some(n) } }
          val newedges = edges mapValues { span => span filterNot { case (f, n) => nodes contains n } }
          /**
           * @todo We need to repeat here the withDefaultValue option.... we probably should consider rewriting
           * stuff so that we do no need them
           */
          val newgraph = Property(newlabels, newedges withDefaultValue Map(), types)
          (newgraph, nodes)
      }
    }

    def testNull(v: Int): Property = testNullWithNodes(v)._1

    def testNotNull(v: Int): Property = {
      if (mustBeNull(v))
        bottom
      else
        this
    }

    def mustBeNull(v: Int, fs: Iterable[om.Field]) = nodeOf(v, fs).isEmpty

    def mayBeNull(v: Int, fs: Iterable[om.Field]) = true

    def mayShare(v1: Int, fs1: Iterable[om.Field], v2: Int, fs2: Iterable[om.Field]) = (nodeOf(v1, fs1), nodeOf(v2, fs2)) match {
      case (Some(n1), Some(n2)) => true
      case _ => false
    }

    def mustShare(v1: Int, fs1: Iterable[om.Field], v2: Int, fs2: Iterable[om.Field]) = false

    def mayBeAliases(v1: Int, v2: Int) = labels(v1).isDefined && labels(v2).isDefined && om.mayBeAliases(types(v1), types(v2))

    def mustBeAliases(v1: Int, v2: Int) = labels(v1).isDefined && labels(v2) == labels(v1)

    def mayBeWeakAliases(v1: Int, v2: Int) = true

    def mustBeWeakAliases(v1: Int, v2: Int) = labels(v1) == labels(v2)
  }
}

object AliasingDomain extends ObjectDomainFactory {
  /**
   * A node in the ALPs graph. We tried to use integer for nodes, but since they
   * are mostly put inside maps, hence they should be boxed, it is not very
   * convenient.
   */
  class Node private (val i: Int) {
    override def toString = i.toString
  }

  /**
   * The factory for nodes.
   */
  object Node {
    var current: Int = 0
    def apply(): Node = {
      current += 1
      new Node(current)
    }
  }

  /**
   * A morphism is a function from Node to Option[Node]. We could use
   * partial function, but I think a standard function is more convenient.
   */
  type Morphism = Function[Node, Option[Node]]

  def apply[OM <: ObjectModel](om: OM) = new AliasingDomain(om)
}
