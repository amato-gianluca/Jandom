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

import scala.util.control.Breaks

/**
 * The domain for the ALPs domain.0
 * @author Gianluca Amato <gamato@unich.it>
 */
class ALPsDomain[OM <: ObjectModel](val om: OM) extends ObjectDomain[OM] {

  def top(types: Seq[om.Type]) = {
    val labels = for { t <- types } yield if (om.mayShare(t, t)) Some(new Node(Some(t))) else None
    for { (t, Some(n)) <- types zip labels; f <- om.fieldsOf(t); tf = om.typeOf(f); if om.mayShare(tf, tf) } n.edges += f -> new Node()
    new Property(types, labels)
  }

  def bottom(types: Seq[om.Type]) = new Property(types, Seq.fill(types.size)(None))

  def apply(types: Seq[om.Type], labels: Seq[Option[Node]]) = new Property(types, labels)

  /**
   * This is a node in the ALPs graph.
   * @param nodeType is the minimum class of the variables bound to this node. None means that
   * there are no variable bound to this node. It could be computed by other informations in a graph,
   * but it should be faster to keep it locally.
   * @param edges is the map from fields to nodes. It is a variable only because it is needed for self loops
   */
  class Node(val nodeType: Option[om.Type] = None, var edges: Map[om.Field, Node] = Map()) {
    /**
     * Expand a node adding all fields in om.Type
     */
    def expand(t: om.Type) = {
      var newedges = edges
      if (nodeType.isEmpty || om.lt(t, nodeType.get)) {
        for (f <- om.fieldsOf(t); if !(edges isDefinedAt f)) newedges += f -> new Node()
        new Node(Some(t), newedges)
      } else
        this
    }
    override def toString = s"n${this.hashCode()}"
  }

  type ALPsMorphism = Map[Node, Option[Node]]

  case class Property(types: Seq[om.Type], labels: Seq[Option[Node]]) extends ObjectProperty[Property] {

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
      labels(i) flatMap { _.edges.get(field) }
    }

    /**
     * Returns the type of the variable i
     */
    def typeOf(i: Int) = types(i)

    def widening(that: Property): Property = ???

    def narrowing(that: Property): Property = ???

    def union(that: Property): Property = ???

    def intersection(that: Property): Property = ???

    def isEmpty: Boolean = false

    def mapVariables(rho: Seq[Int]): Property = ???

    def top: Property = domain.top(fiber)

    def bottom: Property = domain.bottom(fiber)

    def isTop: Boolean = this == top

    def isBottom: Boolean = labels forall { _.isEmpty }

    def connect(other: Property, common: Int): Property = ???

    def fiber = types

    def dimension = labels.size

    def addFreshVariable(t: om.Type): Property = ???

    def addVariable(t: om.Type): Property = ???

    def delVariable(v: Int) = ???

    def assignNull(dst: Int = dimension - 1): Property = {
      Property(types, labels.updated(dst, None))
    }

    def assignVariable(dst: Int, src: Int): Property = {
      labelOf(src) match {
        case None =>
          Property(types, labels.updated(dst, None))
        case Some(src) =>
          val n = src.expand(typeOf(dst))
          Property(types, labels.updated(dst, Some(n)))
      }
    }

    def assignVariableToField(dst: Int, field: om.Field, src: Int): Property = {
      if (isDefiniteNull(dst))
        bottom
      else
        ???
    }

    def assignFieldToVariable(dst: Int, src: Int, field: om.Field): Property = {
      if (isDefiniteNull(src))
        bottom
      else {
        labelOf(src, field) match {
          case None =>
            Property(types, labels.updated(dst, None))
          case Some(src) =>
            val n = src.expand(typeOf(dst))
            // it does not work
            Property(types, labels.updated(dst, Some(n)))
        }
      }
    }

    def isDefiniteNull(v: Int, fieldseq: Seq[om.Field]): Boolean = {
      val loc = labelOf(v)
      (loc, fieldseq) match {
        case (None, _) => true
        case (Some(node), Seq(f)) => node.edges.isDefinedAt(f)
        case _ => false
      }
    }

    def testNull(v: Int): Property = ???

    def testNotNull(v: Int): Property = ???

    /**
     * Returns, if it exists, a morphism connecting `this` and `other`.
     * @param other the other graph. We assume the two graphs are over the same fiber, since
     * we do not check this property.
     * @returns `None` if the two graphs are incomparable, otherwise `Some(i,m)` where `i`
     * is the same result of `tryCompare` and `m` is the morphism, either `this |-> other`
     * or `other |-> this` according to the value of `i`.
     */
    def tryMorphism(other: Property): Option[(Int, ALPsMorphism)] = {

      class MorphismBuilder {
        private var status: Option[Int] = Some(0)
        private var map1 = Map[Node, Option[Node]]()
        private var map2 = Map[Node, Option[Node]]()

        def direction = this.status

        def morphism: Option[ALPsMorphism] = status match {
          case None => None
          case Some(-1) => Some(map2.withDefaultValue(None))
          case _ => Some(map1.withDefaultValue(None))
        }

        def matchFields(n1: Node, n2: Node) = {
          /*
          val fields = (n1.nodeType,n2.nodeType) match {
            case (Some(t1), Some(t2)) => om.fieldsOf(om.min(t1,t2))
            case (Some(t1), None) => om.fieldsOf(t1)
            case (None, Some(t2)) => om.fieldsOf(t2)
            case (None, None) => Seq()
          }
          */
          for ((f, n) <- n1.edges) matchNode(Some(n), n2.edges.get(f))
          for ((f, n) <- n2.edges) matchNode(n1.edges.get(f), Some(n))
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
                  map1 += n1 -> None
                  status = Some(1)
                  matchFields(n1, n2)
                case (false, true) =>
                  map2 += n2 -> None
                  status = Some(-1)
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
      val s2 = for { Some(n) <- labels; (f, ntgt) <- n.edges } yield {
        val l1 = if (hashValued) n.hashCode() else nodenames(n)
        val l2 = if (hashValued) ntgt.hashCode() else nodenames.getOrElseUpdate(ntgt, { currnum += 1; currnum })
        s"n${l1} -- ${f} --> n${l2}"
      }
      s1.mkString("Vars: ", ", ", "") + " // " + s2.mkString("Nodes: ", ", ", "")
    }
  }
}

object ALPsDomain extends ObjectDomainFactory {
  def apply[OM <: ObjectModel](om: OM) = new ALPsDomain(om)
}
