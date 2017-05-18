package fluent.internal

import cats.{ Applicative, Functor, Monoid }
import shapeless.labelled.{ FieldType, field }
import shapeless.ops.record.Selector
import shapeless.{ :+:, ::, CNil, Coproduct, Generic, HList, HNil, Inl, Inr, LabelledGeneric, Lazy }

import scala.util.Try

trait Transformer[A, B] { def apply(a: A): B }

trait ImplicitTransformersPriority1 {
  implicit def hlistGlobalTransformer[A, K, V, T <: HList](implicit
    transformHead: Transformer[A, V],
    transformTail: Transformer[A, T]
  ): Transformer[A, FieldType[K, V] :: T] = new Transformer[A, FieldType[K, V] :: T] {
    override def apply(a: A): ::[FieldType[K, V], T] = field[K](transformHead(a)) :: transformTail(a)
  }

  implicit def emptyMonoidTransformer[F[_], A, B](implicit monoid: Monoid[F[B]]): Transformer[A, F[B]] =
    new Transformer[A, F[B]] {
      override def apply(a: A): F[B] = monoid.empty
    }
}

trait ImplicitTransformersPriority2 extends ImplicitTransformersPriority1 {
  implicit def deepHListTransformer[A, ARepr <: HList, R <: HList, F, K, V, T <: HList](implicit
    generic: LabelledGeneric.Aux[A, ARepr],
    deepHLister: DeepHLister.Aux[ARepr, R],
    selector: Selector.Aux[R, K, F],
    transformHead: Transformer[F, V],
    transformTail: Transformer[A, T]
  ): Transformer[A, FieldType[K, V] :: T] = new Transformer[A, FieldType[K, V] :: T] {
    override def apply(a: A) = {
      val r = deepHLister(generic.to(a))
      val h = transformHead(selector(r))
      field[K](h) :: transformTail(a)
    }
  }
}

trait ImplicitTransformersPriority3 extends ImplicitTransformersPriority2 {
  implicit def hlistTransformer[A, ARepr <: HList, F, K, V, T <: HList](implicit
    generic: LabelledGeneric.Aux[A, ARepr],
    selector: Selector.Aux[ARepr, K, F],
    transformHead: Transformer[F, V],
    transformTail: Transformer[A, T]
  ): Transformer[A, FieldType[K, V] :: T] = new Transformer[A, FieldType[K, V] :: T] {
    override def apply(a: A): ::[FieldType[K, V], T] = {
      val h = transformHead(selector(generic.to(a)))
      field[K](h) :: transformTail(a)
    }
  }

  implicit def fromSealedTransformer[A, Repr <: Coproduct, B](implicit
    generic: Generic.Aux[A, Repr],
    transform: Transformer[Repr, B]
  ): Transformer[A, B] = new Transformer[A, B] {
    override def apply(a: A): B = transform(generic.to(a))
  }

  implicit def fromCoprodTransformer[H, T <: Coproduct, B](implicit
    transformHead: Transformer[H, B],
    transformTail: Transformer[T, B]
  ): Transformer[H :+: T, B] = new Transformer[H :+: T, B] {
    override def apply(a: H :+: T): B = a match {
      case Inl(h) => transformHead(h)
      case Inr(t) => transformTail(t)
    }
  }

  implicit def fromCnilTransformer[A]: Transformer[CNil, A] = new Transformer[CNil, A] {
    override def apply(cnil: CNil): A =
      // there is no CNil instance, so this is never executed
      throw new UnsupportedOperationException("Can't transform CNil")
  }

  implicit def toSealedTransformer[A, Repr <: Coproduct, B](implicit
    generic: Generic.Aux[B, Repr],
    transform: Transformer[A, Repr]
  ): Transformer[A, B] = new Transformer[A, B] {
    override def apply(a: A): B = generic.from(transform(a))
  }

  implicit def toCoprodTransformer[A, H, T <: Coproduct](implicit
    transformHead: Transformer[A, H],
    transformTail: Transformer[A, T]
  ): Transformer[A, H :+: T] = new Transformer[A, H :+: T] {
    override def apply(a: A): H :+: T = Try(Inl(transformHead(a))) getOrElse Inr(transformTail(a))
  }

  implicit def toCnilTransformer[A]: Transformer[A, CNil] = new Transformer[A, CNil] {
    override def apply(a: A): CNil =
      // There is no instance of CNil, so this won't be used
      throw new UnsupportedOperationException("Can't transform into CNil")
  }
}

trait ImplicitTransformersPriority4 extends ImplicitTransformersPriority3 {
  implicit def customTransformer[A, B](implicit f: A => B): Transformer[A, B] = new Transformer[A, B] {
    override def apply(a: A): B = f(a)
  }
  implicit def hlistCustomTransformer[A, K, V, T <: HList](implicit
    f: A => V,
    transformTail: Transformer[A, T]
  ): Transformer[A, FieldType[K, V] :: T] = new Transformer[A, FieldType[K, V] :: T] {
    override def apply(a: A): FieldType[K, V] :: T = field[K](f(a)) :: transformTail(a)
  }

  implicit def genericTransformer[A, B, BRepr <: HList](implicit
    generic: LabelledGeneric.Aux[B, BRepr],
    transform: Lazy[Transformer[A, BRepr]]
  ): Transformer[A, B] = new Transformer[A, B] {
    override def apply(a: A): B = generic.from(transform.value(a))
  }

  implicit def headTransformer[A, ARepr <: HList, F, K, V](implicit
    generic: LabelledGeneric.Aux[A, ARepr],
    selector: Selector.Aux[ARepr, K, F],
    transform: Transformer[F, V]
  ): Transformer[A, V] = new Transformer[A, V] {
    override def apply(a: A): V = transform(selector(generic.to(a)))
  }

  implicit def hnilTransformer[A]: Transformer[A, HNil] = new Transformer[A, HNil] {
    override def apply(a: A): HNil = HNil
  }

  implicit def identityTransformer[A]: Transformer[A, A] = new Transformer[A, A] {
    override def apply(a: A): A = a
  }

  implicit def functorTransformer[F[_], A, B](implicit
    functor: Functor[F],
    transform: Transformer[A, B]
  ): Transformer[F[A], F[B]] = new Transformer[F[A], F[B]] {
    override def apply(a: F[A]): F[B] = functor.map(a)(transform.apply)
  }

  implicit def applicativeTransformer[F[_], A, B](implicit
    applicative: Applicative[F],
    transform: Transformer[A, B]
  ): Transformer[A, F[B]] = new Transformer[A, F[B]] {
    override def apply(a: A): F[B] = applicative.pure(transform(a))
  }

  implicit def emptyToStringTransformer[A](implicit
    generic: Generic.Aux[A, HNil]
  ): Transformer[A, String] = new Transformer[A, String] {
    override def apply(a: A): String = {
      val b = a.toString
      val i = b.indexOf('(')
      if (i >= 0) b.substring(0, i) else b
    }
  }

  implicit def stringToEmptyTransformer[A](implicit
    generic: Generic.Aux[A, HNil],
    transform: Transformer[A, String]
  ): Transformer[String, A] = new Transformer[String, A] {
    override def apply(a: String) = {
      val b = generic.from(HNil)
      if (transform(b) != a) {
        throw new UnsupportedOperationException(s"Can't transform $a into $b")
      }
      b
    }
  }
}

trait ImplicitTransformersPriority5 extends ImplicitTransformersPriority4 {
  implicit def optionExtractorTransformer[A, B](implicit
    transform: Transformer[A, B]
  ): Transformer[Option[A], B] = new Transformer[Option[A], B] {
    override def apply(a: Option[A]): B = transform(a.get)
  }

  implicit def extractorTransformer[A, B](implicit
    generic: Generic.Aux[A, B :: HNil]
  ): Transformer[A, B] = new Transformer[A, B] {
    override def apply(a: A): B = generic.to(a).head
  }
}

object Transformer extends ImplicitTransformersPriority5
