/**
 * Copyright 2013 Gianluca Amato, Francesca Scozzari
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

package it.unich.sci.jandom.domains.numerical

import it.unich.sci.jandom.domains.DomainTransformation

/**
 * This class implements the reduced product of two abstract domains. It is not a
 * real reduced product, but a cartesian product with some reduction given by transformation
 * funtions.
 * @note Due to the bug [[https://issues.scala-lang.org/browse/SI-5712 SI-5712] in Scala 2.10, we had to
 * declare `ProductDomain` an abstract domain. Instatiate it with appropriate values for `dom1`, `dom2`,
 * `dom1Todom2` and `dom2Todom1`.
 * @author Gianluca Amato <gamato@unich.it>
 * @author Francesca Scozzari <fscozzari@unich.it>
 */
 abstract class ProductDomain extends NumericalDomain {
  /**
   * First numerical domain
   */
  val dom1: NumericalDomain

  /**
   * Second numerical domain
   */
  val dom2: NumericalDomain

  /**
   * A transformation from first to second domain.
   */
  val dom1Todom2: DomainTransformation[dom1.Property, dom2.Property]

  /**
   * A transformation from second to first domain
   */
  val dom2Todom1: DomainTransformation[dom2.Property, dom1.Property]

  def top(n: Int) =
    new Property(dom1.top(n), dom2.top(n))

  def bottom(n: Int) =
    new Property(dom1.bottom(n), dom2.bottom(n))

  /**
   * This class represents the reduced product of two base numerical properties.
   * @author Gianluca Amato <gamato@unich.it>
   * @author Francesca Scozzari <fscozzari@unich.it>
   */
  class Property(val p1: dom1.Property, val p2: dom2.Property) extends NumericalProperty[Property] {

    require(p1.dimension == p2.dimension)

    type Domain = ProductDomain.this.type

    def domain = ProductDomain.this

    def reduce(x1: dom1.Property, x2: dom2.Property): Property = {
      if (x1.isEmpty && x2.isEmpty)
        this
      else if (x1.isEmpty)
        new Property(x1, x2.bottom)
      else if (x2.isEmpty)
        new Property(x1.bottom, x2)
      else {
        val y1=x1.intersection(dom2Todom1.apply(x2))
        val y2=x2.intersection(dom1Todom2.apply(x1))

        new Property(y1, y2)
      }
    }

    def union(that: Property): Property = {
      val q1 = p1 union that.p1
      val q2 = p2 union that.p2
      reduce(q1, q2)
    }

    def widening(that: Property): Property =
      // We do not reduce since it may prevent termination
      new Property(this.p1 widening that.p1, this.p2 widening that.p2)

    def narrowing(that: Property): Property =
      // We do not reduce since it may prevent termination
      new Property(this.p1 narrowing that.p1, this.p2 narrowing that.p2)

    def intersection(that: Property): Property = {
      val q1 = p1 intersection that.p1
      val q2 = p2 intersection that.p2
      reduce(q1, q2)
    }

    def nonDeterministicAssignment(n: Int): Property = {
      val q1 = p1.nonDeterministicAssignment(n)
      val q2 = p2.nonDeterministicAssignment(n)
      reduce(q1, q2)
    }

    def linearAssignment(n: Int, lf: LinearForm[Double]): Property = {
      val q1 = p1.linearAssignment(n, lf)
      val q2 = p2.linearAssignment(n, lf)
      reduce(q1, q2)
    }

    def linearInequality(lf: LinearForm[Double]): Property = {
      val q1 = p1.linearInequality(lf)
      val q2 = p2.linearInequality(lf)
      reduce(q1, q2)
    }

    def linearDisequality(lf: LinearForm[Double]): Property = {
      val q1 = p1.linearDisequality(lf)
      val q2 = p2.linearDisequality(lf)
      reduce(q1, q2)
    }

    def minimize(lf: LinearForm[Double]): Double = {
    	val q1=p1.minimize(lf)
    	val q2=p2.minimize(lf)
    	q1 max q2
    }

    def maximize(lf: LinearForm[Double]): Double = {
    	val q1=p1.maximize(lf)
    	val q2=p2.maximize(lf)
    	q1 min q2
    }

    def frequency(lf: LinearForm[Double]): Option[Double] = {
    	p1.frequency(lf) match {
    	  case v@ Some(c) => v
    	  case None => p2.frequency(lf)
    	}
    }

    def addVariable: Property =
      new Property(p1.addVariable, p2.addVariable)

    def delVariable(n: Int): Property =
      new Property(p1.delVariable(n), p2.delVariable(n))

    def mapVariables(rho: Seq[Int]): Property = {
      val q1 = p1.mapVariables(rho)
      val q2 = p2.mapVariables(rho)
      reduce(q1, q2)
    }

    def dimension: Int = p1.dimension

    def isEmpty = p1.isEmpty || p2.isEmpty

    def isTop = p1.isTop && p2.isTop

    def isBottom = isEmpty || (p1.isBottom && p2.isBottom)

    def bottom: Property = ProductDomain.this.bottom(dimension)

    def top: Property = ProductDomain.this.top(dimension)

    def mkString(vars: Seq[String]): String = {
      if (isEmpty)
        "empty"
      else if (isTop)
        "full"
      else
        p1.mkString(vars) + " / " + p2.mkString(vars)
    }

    def tryCompareTo[B >: Property](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] = {
      other match {
        case other: Property => {
          val c1 = p1.tryCompareTo(other.p1)
          val c2 = p2.tryCompareTo(other.p2)
          if (c1 == Some(0))
            c2
          else if (c2 == Some(0))
            c1
          else if (c1 == c2)
            c1
          else
            None
        }
        case _ => None
      }
    }
  }
}
