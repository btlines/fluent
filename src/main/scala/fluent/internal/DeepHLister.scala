package fluent.internal

import shapeless.labelled.FieldType
import shapeless.ops.hlist.Prepend
import shapeless.{ ::, DepFn1, HList, HNil, LabelledGeneric, Lazy }

/**
  * DeepHLister is inspired by https://github.com/milessabin/shapeless/blob/master/examples/src/main/scala/shapeless/examples/deephlister.scala
  * It has been adapted to works with Labelled fields and flatten out the final HList
  */
trait DeepHLister[R <: HList] extends DepFn1[R] { type Out <: HList }

trait LowPriorityDeepHLister {
  type Aux[R <: HList, Out0 <: HList] = DeepHLister[R] { type Out = Out0 }

  implicit def headNotCaseClassDeepHLister[H, T <: HList](
    implicit dht: Lazy[DeepHLister[T]]
  ): Aux[H :: T, H :: dht.value.Out] = new DeepHLister[H :: T] {
    type Out = H :: dht.value.Out
    def apply(r: H :: T): Out = r.head :: dht.value(r.tail)
  }
}

object DeepHLister extends LowPriorityDeepHLister {
  def apply[R <: HList](implicit dh: DeepHLister[R]): Aux[R, dh.Out] = dh

  implicit object hnilDeepHLister extends DeepHLister[HNil] {
    type Out = HNil
    def apply(r: HNil): Out = HNil
  }

  implicit def headCaseClassDeepHLister[K, V, R <: HList, T <: HList, OutR <: HList, OutT <: HList, Out0 <: HList](
    implicit
    gen: LabelledGeneric.Aux[V, R],
    dhh: Lazy[DeepHLister.Aux[R, OutR]],
    dht: Lazy[DeepHLister.Aux[T, OutT]],
    prepend: Prepend.Aux[OutR, OutT, Out0]
  ): Aux[FieldType[K, V] :: T, Out0] = new DeepHLister[FieldType[K, V] :: T] {
      type Out = Out0
      def apply(r: FieldType[K, V] :: T): Out = prepend(dhh.value(gen.to(r.head.asInstanceOf[V])), dht.value(r.tail))
  }
}
