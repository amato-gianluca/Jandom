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

import org.scalatest.FunSpec
import org.scalatest.prop.TableDrivenPropertyChecks
import it.unich.jandom.domains.CartesianFiberedDomainSuite

/**
 * This is a common trait for test suites of object domains. It contains all the standard tests
 * valid for all object domains.
 * @author Gianluca Amato <gamato@unich.it>
 */
trait ObjectDomainSuite extends CartesianFiberedDomainSuite with TableDrivenPropertyChecks {
  this: FunSpec =>

  val om: ObjectModel
  val dom: ObjectDomain[om.type]
  val someProperties: Seq[dom.Property]

  describe("The mayShare method") {
    it("is true when two variables may be aliased") {
      for (p <- someProperties; v1 <- 0 until p.dimension; v2 <- 0 until p.dimension) {
        if (p.mayBeAliases(v1, v2)) assert(p.mayShare(v1, v2), s"${v1} and ${v2} may be aliases but cannot share in ${p}")
      }
    }
    it("is true when two variables must share") {
      for (p <- someProperties; v1 <- 0 until p.dimension; v2 <- 0 until p.dimension) {
        if (p.mustShare(v1, v2)) assert(p.mayShare(v1, v2))
      }
    }
  }

  describe("The assignNull method") {
    val possibleInstances = for {
      p <- someProperties
      i <- 0 until p.dimension
    } yield (p, i)

    val table = Table(("p", "i"), possibleInstances: _*)

    it("makes variables possibly null") {
      forAll(table) { (p, i) =>
        assert(p.assignNull(i).mayBeNull(i))
      }
    }
  }

  describe("The assignVariable method") {
    val possibleInstances = for {
      p <- someProperties
      i <- 0 until p.dimension
      j <- 0 until p.dimension
      if om.lteq(p.typeOf(j), p.typeOf(i))
    } yield (p, i, j)
    val table = Table(("p", "i", "j"), possibleInstances: _*)

    it("makes variables possibly weak aliases") {
      forAll(table) { (p, i, j) =>
        assert(p.assignVariable(i, j).mayBeWeakAliases(i, j))
      }
    }
  }

  describe(s"The assignFieldToVariable method") {
    val possibleInstances = for {
      p <- someProperties
      i <- 0 until p.dimension
      j <- 0 until p.dimension
      f <- om.fieldsOf(p.typeOf(j))
      if om.lteq(om.typeOf(f), p.typeOf(i))
      if !p.mustBeNull(j)
    } yield (p, i, j, f)
    val table = Table(("p", "i", "j", "f"), possibleInstances: _*)

    it(s"makes variables possibly share if source is not null") {
      forAll(table) { (p, i, j, f) =>
        if (!p.mustBeNull(j, Seq(f))) assert(p.assignFieldToVariable(i, j, f).mayShare(i, j) )
      }
    }
    it("propagate possibly nullness") {
      forAll(table) { (p, i, j, f) =>
        if (p.mayBeNull(j, Seq(f))) assert(p.assignFieldToVariable(i, j, f).mayBeNull(i))
      }
    }
    it("propagate possibly non nullness") {
      forAll(table) { (p, i, j, f) =>
        if (!p.mustBeNull(j, Seq(f))) assert(!p.assignFieldToVariable(i, j, f).mustBeNull(i))
      }
    }
  }

  describe("The assignVariableToField method") {
    val possibleInstances = for {
      p <- someProperties
      i <- 0 until p.dimension
      if !p.mustBeNull(i)
      f <- om.fieldsOf(p.typeOf(i))
      j <- 0 until p.dimension
      if om.lteq(p.typeOf(j), om.typeOf(f))
    } yield (p, i, f, j)
    val table = Table(("p", "i", "f", "j"), possibleInstances: _*)

    it("makes variables possibly share if source is not null") {
      forAll(table) { (p, i, f, j) =>
        if (!p.mustBeNull(j)) assert(p.assignVariableToField(i, f, j).mayShare(i, j))
      }
    }
    it("propagate possibly nullness") {
      forAll(table) { (p, i, f, j) =>
        if (p.mayBeNull(j)) assert(p.assignVariableToField(i, f, j).mayBeNull(i, Seq(f)))
      }
    }
    it("propagate possibly non nullness if destination is not null") {
      forAll(table) { (p, i, f, j) =>
        if (!p.mustBeNull(j))
          assert(!p.assignVariableToField(i, f, j).mustBeNull(i, Seq(f)))
      }
    }
  }

  describe("The cast method") {
    // checks may be made more precise my improving the selection of types 
    // to use for casting
    val possibleInstances = for {
      p <- someProperties
      i <- 0 until p.dimension
      t = p.typeOf(i)
    } yield (p, i, t)
    val table = Table(("p", "i", "t"), possibleInstances: _*)

    it("propagate possibly nullness") {
      forAll(table) { (p, i, t) =>
        if (p.mayBeNull(i)) assert(p.castVariable(i, t).mayBeNull(i))
      }
    }
    it("propagate possibly non-nullness") {
      forAll(table) { (p, i, t) =>
        if (!p.mustBeNull(i)) assert(!p.castVariable(i, t).mustBeNull(i))
      }
    }
  }

  describe("The addFreshVariable method") {
    val possibleInstances = for {
      p <- someProperties
      t <- p.fiber
    } yield (p, t)
    val table = Table(("p", "t"), possibleInstances: _*)

    it("add a variable") {
      forAll(table) { (p, t) => assert(p.addFreshVariable(t).dimension === p.dimension + 1) }
    }

    it("adds a variable which may not share, not alias and not be null") {
      forAll(table) { (p, t) =>
        val p2 = p.addFreshVariable(t)
        assert(!p2.mustBeNull(p.dimension))
        assert(p2.dimension === p.dimension + 1)
        for (j <- 0 until p.dimension) {
          assert(!p2.mustShare(p.dimension, j))
          assert(!p2.mustBeAliases(p.dimension, j))
        }
      }
    }
  }

  describe("The addUnknownVariable method") {
    val possibleInstances = for {
      p <- someProperties
      t <- p.fiber
    } yield (p, t)
    val table = Table(("p", "t"), possibleInstances: _*)

    it("add a variable") {
      forAll(table) { (p, t) => assert(p.addUnknownVariable(t).dimension === p.dimension + 1) }
    }
  }
}

object ObjectDomainSuite {

  /**
   * This is an object model used in several tests.
   * @author Gianluca Amato <gamato@unich.it>
   */
  object TestObjectModel extends ObjectModel with ObjectModel.NoArrays {
    type Type = AnyRef
    type Field = Char

    val tother = "tother"
    val tsuper = "tsuper"
    val tsub = "tsub"

    def lteq(t1: Type, t2: Type) = t1 == this.tsub || t2 == this.tsuper
    def glbApprox(ts: Iterable[Type]) = if (ts.isEmpty)
      None
    else if (ts exists { _ == tsub })
      Some(tsub)
    else
      Some(tsuper)
    def mayShare(t1: Type, t2: Type) = true
    def mayBeAliases(t1: Type, t2: Type) = true
    def fieldsOf(t: Type) = t match {
      case `tsuper` => Set('a', 'b')
      case `tsub` => Set('a', 'b', 'c')
      case `tother` => Set()
    }
    def typeOf(f: Field) = f match {
      case 'a' => tsuper
      case 'b' => tsuper
      case 'c' => tsub
    }
  }
}
