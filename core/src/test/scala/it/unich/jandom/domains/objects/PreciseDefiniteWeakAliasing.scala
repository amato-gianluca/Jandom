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

/**
 * A trait for testing object domains which precisely implements definite weak aliasing
 * @author Gianluca Amato <gamato@unich.it>
 *
 */
trait PreciseDefiniteWeakAliasing extends ObjectDomainSuite {

  describe("The bottom element") {
    it("has all variables definitively weak aliased") {
      forAll(someFibersAndTwoVars) { (fiber, i, j) =>
          assert(dom.bottom(fiber).mustBeWeakAliases(i, j))
      }
    }
  }

  describe("The assignVariable method") {
    it("makes variables definitively aliases") {
      forAll(someAssignVariable) { (p, dst, src) =>
        assert(p.assignVariable(dst, src).mustBeWeakAliases(dst, src))
      }
    }
  }
}