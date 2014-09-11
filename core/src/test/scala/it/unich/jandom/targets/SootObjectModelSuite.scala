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

import org.scalatest.FunSpec
import org.scalatest.PrivateMethodTester

import it.unich.jandom.targets.jvmsoot.SootObjectModel
import it.unich.jandom.domains.objects.ObjectModelSuite

import soot._

class SootObjectModelSuite extends FunSpec with ObjectModelSuite with SootTests {

  val klassA = RefType.v(scene.loadClassAndSupport("javatest.A"))
  val klassB = RefType.v(scene.loadClassAndSupport("javatest.B"))
  val klassListA = RefType.v(scene.loadClassAndSupport("javatest.ListA"))
  val klassListA2 = RefType.v(scene.loadClassAndSupport("javatest.ListA2"))
  val klassListA3 = RefType.v(scene.loadClassAndSupport("javatest.ListA3"))
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
  val arrPrimitive = ArrayType.v(primitiveInt, 2)
  val arrS3dim2 = ArrayType.v(klassS3, 2)
  val arrS3dim1 = ArrayType.v(klassS3, 1)
  val arrS1dim1 = ArrayType.v(klassS1, 1)
  val arrIface = ArrayType.v(interfaceList, 2)
  val arrIface2 = ArrayType.v(interfaceList2, 2)

  val someTypes = Seq(klassA, klassB, klassListA, klassPair, klassS1, klassS2, klassS3, klassS4,
    klassS5, klassR3, interfaceList, interfaceList, interfaceList2, interfaceList3, interfaceOther, klassK,
    primitiveInt, primitiveByte, arrPrimitive, arrS3dim2, arrS3dim1, arrS1dim1, arrIface, arrIface2,
    klassListA2, klassListA3)

  val om = new SootObjectModel(scene)

  /**
   * describe("The type of all the fields") {
   * it("are different for different definition points") {
   * val f1 = klassPairgetSootClass().getField("v", klassA)
   * val f2 = klassListA.getSootClass().getField("v", klassA)
   * assert(f1 != f2)
   * assert((om.fieldsOf(klassPair) intersect om.fieldsOf(klassListA)).isEmpty)
   * }
   * }
   */

  describe("The subtype relation") {

    it("passes some specific test for SootObjectModel") {
      assert(om.lteq(klassS3, klassS1))
      assert(om.lteq(klassS2, klassS1))
      assert(om.lteq(klassS3, klassS2))
      assert(om.lteq(klassListA, interfaceList))
      assert(om.lteq(interfaceList2, interfaceList))
      assert(om.lteq(primitiveInt, primitiveInt))
      assert(!om.lteq(primitiveByte, primitiveInt))
    }

    it("is covariant on ArrayType") {
      assert(om.lteq(arrS3dim1, arrS1dim1))
    }
  }

  describe("The glbapprox relation") {  
    it("passes some specific test for SootObjectModel") {
      assert(om.glbApprox(Seq(interfaceList, interfaceOther)) === Some(scene.getObjectType()))
      assert(om.glbApprox(Seq(interfaceList, interfaceOther, klassListA)) === Some(klassListA))
      assert(om.glbApprox(Seq(interfaceList, klassA, interfaceOther, klassListA)) === None)
    }
  }

  describe("The set of needed fields of a type") {

    it("is anti-monotone w.r.t. subtype") {
      for (t1 <- someTypes; t2 <- someTypes; if om.lteq(t1, t2)) {
        val fields1 = om.getNeededFields(t1)
        val fields2 = om.getNeededFields(t2)
        assert(fields2 subsetOf fields1, s"${t1} has needed fields ${fields1}, {t1}<=${t2} but ${t2} has needed fields ${fields2}")
      }
    }

    it("is empty on primitive types") {
      for (t <- someTypes; if t.isInstanceOf[PrimType])
        assert(om.getNeededFields(t).isEmpty)
    }

    it("is empty on interfaces") {
      for (t <- someTypes; if t.isInstanceOf[RefType] && t.asInstanceOf[RefType].getSootClass().isInterface())
        assert(om.getNeededFields(t).isEmpty)
    }

    it("passes some specific test for SootObjectModel") {
      val f1 = klassS2.getSootClass().getField("f1", klassA)
      val f2 = klassS3.getSootClass().getField("f2", klassB)
      assertResult(Set(f1, f2)) { om.getNeededFields(klassS3) }
      assertResult(Set(f1, f2)) { om.getNeededFields(klassS3) }
    }
  }

  describe("The possible fields of a class/interface") {
    it("is bigger then needed fields") {
      for (t <- someTypes) {
        val neededFields = om.getNeededFields(t)
        val possibleFields = om.getPossibleFields(t)
        assert(neededFields subsetOf possibleFields)
      }
    }

    it("is monotone w.r.t. subtype") {
      for (t1 <- someTypes; t2 <- someTypes; if om.lteq(t1, t2)) {
        val fields1 = om.getPossibleFields(t1)
        val fields2 = om.getPossibleFields(t2)
        assert(fields1 subsetOf fields2, s"${t1} has possible fields ${fields1}, {t1}<=${t2} but ${t2} has possible fields ${fields2}")
      }
    }

    it("is empty on primitive types") {
      for (t <- someTypes; if om.isPrimitive(t))
        assert(om.getPossibleFields(t).isEmpty)
    }

    it("passes some specific test for SootObjectModel") {
      val f1 = klassS2.getSootClass().getField("f1", klassA)
      val f2 = klassS3.getSootClass().getField("f2", klassB)
      val l = klassS4.getSootClass().getField("l", klassListA)
      assertResult(Set(f1, f2)) { om.getPossibleFields(klassS3) }
      assertResult(Set(f1, f2, l)) { om.getPossibleFields(klassS2) }
    }
  }

  describe("The instantiability of a type") {
    it("is upward closed") {
      for (t1 <- someTypes; t2 <- someTypes) {
        if (om.typeMayBeInstantiated(t1) && om.lteq(t1, t2))
          assert(om.typeMayBeInstantiated(t2))
      }
    }
    it("passes some specific test for SootObjectModel") {
      assert(om.typeMayBeInstantiated(interfaceList))
      assert(!om.typeMayBeInstantiated(interfaceList2))
      assert(om.typeMayBeInstantiated(interfaceList3))
      assert(!om.typeMayBeInstantiated(primitiveInt))
      assert(om.typeMayBeInstantiated(klassPair))
      assert(om.typeMayBeInstantiated(arrIface))
      assert(om.typeMayBeInstantiated(arrIface2))
    }
  }

  describe("Type reachability") {
    it("is reflexive for types which may reach with other types") {
      for (t1 <- someTypes; t2 <- someTypes; if om.mayShare(t1, t2)) {
        assert(om.reachableFrom(t1, t1))
      }
    }

    it("is transitive") {
      for (t1 <- someTypes; t2 <- someTypes; t3 <- someTypes) {
        if (om.reachableFrom(t1, t2) && om.reachableFrom(t2, t3)) {
          assert(om.reachableFrom(t1, t3), s"${t1} may reach ${t2} which may reach ${t3}, but ${t1} cannot reach ${t3}")
        }
      }
    }

    it("is downward closed on the target") {
      for (t1 <- someTypes; t2 <- someTypes; t3 <- someTypes) {
        if (om.reachableFrom(t1, t2) && om.lteq(t3, t2)) {
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

    it("passes some specific test for SootObjectModel") {
      assert(!om.reachableFrom(interfaceList2, interfaceList2))
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
    it("is reflexive for types which shares with other types") {
      for (t1 <- someTypes; t2 <- someTypes; if om.mayShare(t1, t2)) {
        assert(om.mayShare(t1, t1))
      }
    }

    it("is symmetric") {
      for (t1 <- someTypes; t2 <- someTypes; if om.mayShare(t1, t2)) {
        assert(om.mayShare(t2, t1))
      }
    }

    it("is upward closed") {
      for (t1 <- someTypes; t2 <- someTypes; t3 <- someTypes; if om.mayShare(t1, t2)) {
        if (om.lteq(t2, t3)) assert(om.mayShare(t1, t3), s"${t1} share with ${t2} and ${t1} <= ${t3}")
        if (om.lteq(t1, t3)) assert(om.mayShare(t2, t3), s"${t1} share with ${t2} and ${t2} <= ${t3}")
      }
    }

    it("passes some specific test for SootObjectModel") {
      assert(om.mayShare(klassA, klassA))
      assert(!om.mayShare(klassA, klassB))
      assert(om.mayShare(klassA, klassListA))
      assert(!om.mayShare(klassB, klassListA))
      assert(om.mayShare(klassA, klassPair))
      assert(om.mayShare(klassListA, klassPair))
      assert(om.mayShare(klassB, klassPair))
      assert(om.mayShare(interfaceList, interfaceOther))
    }
  }
}

