package it.unich.jandom.utils.parametermap

object TLists {
  sealed trait TList
  object TNil extends TList
  final class TCons[H, T <: TList] extends TList

  type TNil = TNil.type
  type ::[H, T <: TList] = TCons[H, T]

  def value[T] = null.asInstanceOf[T]
  
  final class Contains[H, T <: TList]()  
  implicit def contains[H, T <: TList] = value[Contains[H, H :: T]]
  implicit def contains2[H, H2, T <: TList](implicit ev1: Contains[H, T]) = value[Contains[H, H2 :: T]]

  final class SubSet[S <: TList, T <: TList]()
  implicit def subset[T <: TList] = value[SubSet[TNil, T]]
  implicit def subset2[H, S <: TList, T <: TList](implicit ev1: Contains[H, T], ev2: SubSet[S,T]) = value[SubSet[H :: S, T]]
}
