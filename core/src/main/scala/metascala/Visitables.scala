package metascala

object Visitables {
  import scala.language.higherKinds
  
  trait TypeVisitor {
    type ResultType
  }

  trait Visitable[V <: TypeVisitor] {
    type Accept[V2 <: V] <: V2#ResultType
  }
}