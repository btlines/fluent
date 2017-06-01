package fluent

trait TransformError {
  def message: String
}

object TransformError {
  def apply(error: String): TransformError = TransformErrorMessage(error)
  def apply(error: Throwable): TransformError = TransformErrorThrowable(error)

  case class TransformErrorMessage(message: String) extends TransformError

  case class TransformErrorThrowable(throwable: Throwable) extends TransformError {
    override def message: String = throwable.getMessage
  }
}
