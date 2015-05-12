package metascala


object Booleans {   
  import scala.language.higherKinds
  import metascala.Utils._

  sealed trait Bool {
    type And[B <: Bool] <: Bool
    type Or[B <: Bool] <: Bool
    type Not <: Bool
    type If[IfT, IfF]
    type If2[T, IfT <: T, IfF <: T] <: T
  }
  
  final class True extends Bool {
    type And[B <: Bool] = B
    type Or[B <: Bool] = True
    type Not = False
    type If[IfT, IfF] = IfT
    type If2[T, IfT <: T, IfF <: T] = IfT
  }
  
  val True = new True

  final class False extends Bool {
    type And[B <: Bool] = False
    type Or[B <: Bool] = B
    type Not = True
    type If[IfT, IfF] = IfF
    type If2[T, IfT <: T, IfF <: T] = IfF
  }
  
  val False = new False
  
  type &&[B1 <: Bool, B2 <: Bool] = B1#And[B2]
  type ||[B1 <: Bool, B2 <: Bool] = B1#Or[B2]

  
  implicit val falseToBoolean = TypeToValue[False, Boolean](false)
  implicit val trueToBoolean = TypeToValue[True, Boolean](true)

  trait IfTrue[P >: True <: True, T] {
    type Type = T
  }
 
}
