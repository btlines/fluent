import fluent.internal.Transformer

package object fluent {
  implicit class TransformerOps[A](a: A) {
    def transformTo[B](implicit transform: Transformer[A, B]): B = transform(a) match {
      case Right(b) => b
      case Left(TransformError.TransformErrorThrowable(error)) =>  throw error
      case Left(error) => throw new IllegalArgumentException(s"Can't transform $a: ${error.message}")
    }
    def changeTo[B](implicit transformer: Transformer[A, B]): Either[TransformError, B] = transformer(a)
  }
}
