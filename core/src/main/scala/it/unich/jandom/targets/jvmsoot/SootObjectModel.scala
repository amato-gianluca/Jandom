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

package it.unich.jandom.targets.jvmsoot

import it.unich.jandom.domains.objects.ObjectModel

/**
 * @author Gianluca Amato <gamato@unich.it>
 *
 */
class SootObjectModel(cra: SootClassReachableAnalysis) extends ObjectModel {
   import scala.collection.JavaConversions._

   type Type = soot.Type
   type Field = soot.SootField
   def mayShare(i: Type, j: Type) =
     (i,j) match {
        case (p1: soot.RefType, p2: soot.RefType) => cra.mayShare(p1.getSootClass(), p2.getSootClass())
        case _ => false
   }

   def fieldsOf(i: Type) = i match {
     case i: soot.RefType => i.getSootClass().getFields().toSet
     case _ => Set()
   }

   def typeOf(f: Field) = f.getType()

   def lt(t1: Type, t2: Type) = ???
}
