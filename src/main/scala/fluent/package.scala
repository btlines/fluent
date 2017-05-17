import fluent.internal.Transformer

package object fluent {
  implicit class TransformerOps[A](a: A) {
    def transformTo[B](implicit transformer: Transformer[A, B]): B = transformer(a)
  }
}
