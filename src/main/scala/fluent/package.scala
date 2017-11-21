import fluent.internal.Transformer
import shapeless.ops.hlist.Prepend
import shapeless.{HList, LabelledGeneric, RecordArgs}

package object fluent {
  implicit class TransformerOps[A](a: A) {
    def transformTo[B](implicit transform: Transformer[A, B]): B = transform(a) match {
      case Right(b) => b
      case Left(TransformError.TransformErrorThrowable(error)) =>  throw error
      case Left(error) => throw new IllegalArgumentException(s"Can't transform $a: ${error.message}")
    }

    def changeTo[B](implicit transformer: Transformer[A, B]): Either[TransformError, B] = transformer(a)

    object using extends RecordArgs {
      def applyRecord[ARepr <: HList, Rec <:HList](
        rec: Rec
    )(implicit gen: LabelledGeneric.Aux[A, ARepr], prepend: Prepend[Rec, ARepr]) =
        // rec goes first as its fields should take precedence over ARepr
        // so that it can be used to override some fields present in A
        prepend(rec, gen.to(a))
    }
  }
}
