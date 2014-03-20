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

package it.unich.jandom.targets

import soot._
import it.unich.jandom.domains.ObjectModelSuite
import it.unich.jandom.targets.jvmsoot.SootObjectModel
import org.scalatest.FunSpec
import org.scalatest.PrivateMethodTester

class SootObjectModelSuite extends FunSpec with ObjectModelSuite with PrivateMethodTester with SootTests {

  val klassA = RefType.v(scene.loadClassAndSupport("javatest.A"))
  val klassB = RefType.v(scene.loadClassAndSupport("javatest.B"))
  val klassListA = RefType.v(scene.loadClassAndSupport("javatest.ListA"))
  val interfaceList = RefType.v(scene.loadClassAndSupport("javatest.ListInterface"))
  val interfaceList2 = RefType.v(scene.loadClassAndSupport("javatest.ListInterface2"))
  val interfaceList3 = RefType.v(scene.loadClassAndSupport("javatest.ListInterface3"))
  val interfaceOther = RefType.v(scene.loadClassAndSupport("javatest.OtherInterface"))
  val klassPair = RefType.v(scene.loadClassAndSupport("javatest.Pair"))
  val klassS1 = RefType.v(scene.loadClassAndSupport("javatest.S1"))
  val klassS2 = RefType.v(scene.loadClassAndSupport("javatest.S2"))
  val klassS3 = RefType.v(scene.loadClassAndSupport("javatest.S3"))
  val klassS4 = RefType.v(scene.loadClassAndSupport("javatest.S4"))
  val klassS5 = RefType.v(scene.loadClassAndSupport("javatest.S5"))
  val klassR3 = RefType.v(scene.loadClassAndSupport("javatest.R3"))
  val klassK = RefType.v(scene.loadClassAndSupport("javatest.K"))
  val primitiveInt = IntType.v()
  val primitiveByte = ByteType.v()

  val someTypes = Seq(klassA, klassB, klassListA, klassPair, klassS1, klassS2, klassS3, klassS4,
    klassS5, klassR3, interfaceList, interfaceList, interfaceList2, interfaceList3, interfaceOther, klassK,
    primitiveInt, primitiveByte)

  val om = new SootObjectModel(scene)

  describe("it passes the following tests") {
    it("Classes have correct number of fields") {
      assert ( om.fieldsOf(klassPair).size === 2 )
      assert ( om.fieldsOf(klassS2).size === 1 )
      assert ( om.fieldsOf(klassS3).size === 2 )
      assert ( om.fieldsOf(primitiveInt).size === 0 )
      assert ( om.fieldsOf(primitiveByte).size === 0 )
      assert ( om.fieldsOf(klassA).size === 0 )
    }
    it("Subype relation is correct") {
      assert ( om.lteq(klassS3, klassS1) )
      assert ( om.lteq(klassS2, klassS1) )
      assert ( om.lteq(klassS3, klassS2) )
      assert ( om.lteq(klassListA, interfaceList) )
    }
    it("Fields have the right types") {
      assert ( om.fieldsOf(klassPair).map { om.typeOf(_) } === Set(klassA, klassB) )
      assert ( om.fieldsOf(klassS3).map { om.typeOf(_) } === Set(klassA, klassB) )
    }
    it("Field of the same name in different classes are different") {
      val f1 = klassPair.getSootClass().getField("v",klassA)
      val f2 = klassListA.getSootClass().getField("v",klassA)
      assert (f1 != f2)
      assert ( (om.fieldsOf(klassPair) intersect om.fieldsOf(klassListA)).isEmpty )
    }
    it("Possible sharing information is correct") {
      assert (om.mayShare(klassA, klassA))
      assert (! om.mayShare(klassA, klassK))
      assert (om.mayShare(klassA, klassListA))
      assert (om.mayShare(klassA, klassPair))
      assert (om.mayShare(klassB, klassPair))
      assert (om.mayShare(klassS2, klassA))
      assert (om.mayShare(klassS3, klassA))
      assert (om.mayShare(klassA, klassS2))
      assert (om.mayShare(klassA, klassS3))
      // TODO deal with interfaces
      // assert (om.mayShare(interfaceList, interfaceList))
      // assert (om.mayShare(interfaceList, classListA))
    }
  }

  describe("Crown type reachability") {
    val reachablesFrom = PrivateMethod[Set[Type]]('reachablesFrom)

    it("contains itself") {
      for (t <- someTypes) {
        assert((om invokePrivate reachablesFrom(t)) contains t)
      }
    }

    it("is transitive") {
      for (t1 <- someTypes; t2 <- someTypes; t3 <- someTypes) {
        val reach1 = om invokePrivate reachablesFrom(t1)
        val reach2 = om invokePrivate reachablesFrom(t2)
        if ((reach1 contains t2) && (reach2 contains t3)) {
          assert((om invokePrivate reachablesFrom(t1)) contains t3)
        }
      }
    }

    it("passes some specific tests") {
      assertResult(Set(klassA))(om invokePrivate reachablesFrom(klassA))
      assertResult(Set(klassB))(om invokePrivate reachablesFrom(klassB))
      assertResult(Set(klassA, klassListA))(om invokePrivate reachablesFrom(klassListA))
      assertResult(Set(klassA, klassB, klassPair))(om invokePrivate reachablesFrom(klassPair))
      assertResult(Set(klassS3, klassA, klassB))(om invokePrivate reachablesFrom(klassS3))
      assertResult(Set(klassS2, klassA, klassB, klassListA))(om invokePrivate reachablesFrom(klassS2))
      assertResult(Set(klassR3, klassS3, klassA, klassB))(om invokePrivate reachablesFrom(klassR3))
    }
  }

  describe("Type reachability") {

    it("is reflexive") {
      for (t <- someTypes) {
        assert(om.reachableFrom(t, t))
      }
    }

    it("is transitive") {
      for (t1 <- someTypes; t2 <- someTypes; t3 <- someTypes) {
        if (om.reachableFrom(t1, t2) && om.reachableFrom(t2, t3)) {
          assert(om.reachableFrom(t1, t3))
        }
      }
    }

    it("is downward closed on the target") {
      for (t1 <- someTypes; t2 <- someTypes; t3 <- someTypes) {
        if (om.reachableFrom(t1, t2) && om.lteq(t3,t2)) {
          assert(om.reachableFrom(t1, t3))
        }
      }
    }

    it("is upward closed on the source") {
      for (t1 <- someTypes; t2 <- someTypes; t3 <- someTypes) {
        if (om.reachableFrom(t1, t2) && om.lteq(t1, t3)) {
          assert(om.reachableFrom(t3, t2))
        }
      }
    }

    it("passes some specific tests") {
      assert(om.reachableFrom(klassA, klassA))
      assert(!om.reachableFrom(klassA, klassB))
      assert(om.reachableFrom(klassListA, klassA))
      assert(om.reachableFrom(klassR3, klassS5))
      assert(!om.reachableFrom(klassR3, klassS4))
      assert(om.reachableFrom(klassR3, klassS3))
      assert(!om.reachableFrom(klassR3, klassS2))
    }
  }

  describe("Type sharing") {
    it("is reflexive") {
      for (t <- someTypes) {
        assert(om.mayShare(t, t))
      }
    }

    it("is symmetric") {
      for (t1 <- someTypes; t2 <- someTypes; if om.mayShare(t1,t2)) {
        assert(om.mayShare(t2, t1))
      }
    }

    it("is upward closed") {
      for (t1 <- someTypes; t2 <- someTypes; t3 <- someTypes; if om.mayShare(t1, t2)) {
        if (om.lteq(t2, t3)) assert(om.mayShare(t1, t3), s"${t1} share with ${t2} and ${t1} <= ${t3}")
        if (om.lteq(t1, t3)) assert(om.mayShare(t2, t3), s"${t1} share with ${t2} and ${t2} <= ${t3}")
      }
    }

    it("passes some specific tests") {
      assert(om.mayShare(klassA, klassA))
      assert(!om.mayShare(klassA, klassB))
      assert(om.mayShare(klassA, klassListA))
      assert(!om.mayShare(klassB, klassListA))
      assert(om.mayShare(klassA, klassPair))
      assert(om.mayShare(klassListA, klassPair))
      assert(om.mayShare(klassB, klassPair))
    }
  }
}

