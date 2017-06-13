package fluent.internal

import cats.{Applicative, Monoid, Traverse}
import fluent.TransformError
import shapeless.labelled.{FieldType, field}
import shapeless.ops.record.Selector
import shapeless.{:+:, ::, CNil, Coproduct, Generic, HList, HNil, Inl, Inr, LabelledGeneric, Lazy}

import scala.util.Try

trait Transformer[A, B] { def apply(a: A): Either[TransformError, B] }

trait ImplicitTransformersPriority1 {
  def instance[A, B](f: A => Either[TransformError, B]): Transformer[A, B] =
    new Transformer[A, B] {
      override def apply(a: A): Either[TransformError, B] = f(a)
    }

  implicit def hlistGlobalTransformer[A, K, V, T <: HList](implicit
    transformHead: Transformer[A, V],
    transformTail: Transformer[A, T]
  ): Transformer[A, FieldType[K, V] :: T] = instance { a: A =>
    for {
      h <- transformHead(a)
      t <- transformTail(a)
    } yield field[K](h) :: t
  }

  implicit def emptyMonoidTransformer[F[_], A, B](implicit monoid: Monoid[F[B]]): Transformer[A, F[B]] =
    instance { _: A =>
      Right(monoid.empty)
    }
}

trait ImplicitTransformersPriority2 extends ImplicitTransformersPriority1 {
  implicit def deepHListTransformer[A, ARepr <: HList, R <: HList, F, K, V, T <: HList](implicit
    generic: LabelledGeneric.Aux[A, ARepr],
    deepHLister: DeepHLister.Aux[ARepr, R],
    selector: Selector.Aux[R, K, F],
    transformHead: Transformer[F, V],
    transformTail: Transformer[A, T]
  ): Transformer[A, FieldType[K, V] :: T] = instance { a: A =>
    val r = deepHLister(generic.to(a))
    for {
      h <- transformHead(selector(r))
      t <- transformTail(a)
    } yield field[K](h) :: t
  }
}

trait ImplicitTransformersPriority3 extends ImplicitTransformersPriority2 {
  implicit def hlistTransformer[A, ARepr <: HList, F, K, V, T <: HList](implicit
    generic: LabelledGeneric.Aux[A, ARepr],
    selector: Selector.Aux[ARepr, K, F],
    transformHead: Transformer[F, V],
    transformTail: Transformer[A, T]
  ): Transformer[A, FieldType[K, V] :: T] = instance { a: A =>
    for {
      h <- transformHead(selector(generic.to(a)))
      t <- transformTail(a)
    } yield field[K](h) :: t
  }

  implicit def fromSealedTransformer[A, Repr <: Coproduct, B](implicit
    generic: Generic.Aux[A, Repr],
    transform: Transformer[Repr, B]
  ): Transformer[A, B] = instance { a: A =>
    transform(generic.to(a))
  }

  implicit def fromCoprodTransformer[H, T <: Coproduct, B](implicit
    transformHead: Transformer[H, B],
    transformTail: Transformer[T, B]
  ): Transformer[H :+: T, B] = instance { a: H :+: T =>
    a match {
      case Inl(h) => transformHead(h)
      case Inr(t) => transformTail(t)
    }
  }

  implicit def fromCnilTransformer[A]: Transformer[CNil, A] = instance { _ =>
    // there is no CNil instance, so this is never executed
    Left(TransformError("Can't transform CNil"))
  }

  implicit def toSealedTransformer[A, Repr <: Coproduct, B](implicit
    generic: Generic.Aux[B, Repr],
    transform: Transformer[A, Repr]
  ): Transformer[A, B] = instance { a: A =>
    transform(a) map generic.from
  }

  implicit def toCoprodTransformer[A, H, T <: Coproduct](implicit
    transformHead: Transformer[A, H],
    transformTail: Transformer[A, T]
  ): Transformer[A, H :+: T] = instance { a: A =>
    transformHead(a).map(Inl.apply) match {
      case h@Right(_) => h
      case _ => transformTail(a).map(Inr.apply)
    }
  }

  implicit def toCnilTransformer[A]: Transformer[A, CNil] = instance { a: A =>
    // There is no instance of CNil, so this won't be used
    Left(TransformError("Can't transform into CNil"))
  }
}

trait ImplicitTransformersPriority4 extends ImplicitTransformersPriority3 {
  implicit def customTransformer[A, B](implicit f: A => B): Transformer[A, B] =
    instance { a: A =>
      Try(f(a)).toEither.left.map(TransformError.apply)
    }
  implicit def hlistCustomTransformer[A, K, V, T <: HList](implicit
    f: A => V,
    transformTail: Transformer[A, T]
  ): Transformer[A, FieldType[K, V] :: T] = instance { a: A =>
    for {
      h <- Try(f(a)).toEither.left.map(TransformError.apply)
      t <- transformTail(a)
    } yield field[K](h) :: t
  }

  implicit def genericTransformer[A, B, BRepr <: HList](implicit
    generic: LabelledGeneric.Aux[B, BRepr],
    transform: Lazy[Transformer[A, BRepr]]
  ): Transformer[A, B] = instance { a: A =>
    transform.value(a) map generic.from
  }

  implicit def headTransformer[A, ARepr <: HList, F, K, V](implicit
    generic: LabelledGeneric.Aux[A, ARepr],
    selector: Selector.Aux[ARepr, K, F],
    transform: Transformer[F, V]
  ): Transformer[A, V] = instance { a: A =>
    transform(selector(generic.to(a)))
  }

  implicit def hnilTransformer[A]: Transformer[A, HNil] = instance {
    _: A =>Right(HNil)
  }

  implicit def identityTransformer[A]: Transformer[A, A] = instance {
    a: A => Right(a)
  }

  implicit def functorTransformer[M[_], A, B](implicit
    transform: Transformer[A, B],
    traverse: Traverse[M],
    applicative: Applicative[({type λ[X] = Either[TransformError, X]})#λ]
  ): Transformer[M[A], M[B]] = instance { a: M[A] =>
    applicative.traverse(a)(x => transform(x))
  }

  implicit def applicativeTransformer[F[_], A, B](implicit
    applicative: Applicative[F],
    transform: Transformer[A, B]
  ): Transformer[A, F[B]] = instance { a: A =>
    transform(a).map(applicative.pure)
  }

  implicit def emptyToStringTransformer[A](implicit
    generic: Generic.Aux[A, HNil]
  ): Transformer[A, String] = instance { a: A =>
    val b = a.toString
    val i = b.indexOf('(')
    if (i >= 0) Right(b.substring(0, i)) else Right(b)
  }

  implicit def stringToEmptyTransformer[A](implicit
    generic: Generic.Aux[A, HNil],
    transform: Transformer[A, String]
  ): Transformer[String, A] = instance { a: String =>
    val b = generic.from(HNil)
    transform(b) match {
      case Right(`a`)  => Right(b)
      case Right(_)    => Left(TransformError(s"Can't transform '$a' into $b"))
      case Left(error) => Left(error)
    }
  }
}

trait ImplicitTransformersPriority5 extends ImplicitTransformersPriority4 {
  implicit def optionExtractorTransformer[A, B](implicit
    transform: Transformer[A, B]
  ): Transformer[Option[A], B] = instance { a: Option[A] =>
    a.map(transform.apply) getOrElse Left(TransformError("Missing required field"))
  }

  implicit def extractorTransformer[A, B](implicit
    generic: Generic.Aux[A, B :: HNil]
  ): Transformer[A, B] = instance { a: A =>
    Right(generic.to(a).head)
  }
}

object Transformer extends ImplicitTransformersPriority5
