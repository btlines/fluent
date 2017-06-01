package fluent

import java.time.Instant

import org.scalatest.{ Matchers, WordSpecLike }

object External {
  case class Circle(x: Double, y: Double, radius: Double, color: Option[String])
  case class Post(author: String, body: String, timestamp: Long)
  case class PostWithOptionalFields(author: Option[String], body: Option[String], timestamp: Option[Long])
}

object Internal {
  case class Point(x: Double, y: Double)
  sealed trait Color
  object Color {
    case object Blue extends Color
    case object Red extends Color
    case object Yellow extends Color
  }
  case class Circle(center: Point, radius: Double, color: Option[Color])
  case class Cylinder(origin: Point, radius: Double, height: Double, color: Color)

  case class Author(author: String)
  case class Post(author: Author, body: String, tags: List[String], timestamp: Instant)
  case class Published(post: Post)
}

class FluentSpec extends WordSpecLike with Matchers {

  // Needed to support transformation on Option and List
  import cats.instances.option._
  import cats.instances.list._

  "Fluent" should {
    val externalCircle = External.Circle(
      x = 1.0,
      y = 2.0,
      radius = 3.0,
      color = Some("Red")
    )
    val internalCircle = Internal.Circle(
      center = Internal.Point(1.0, 2.0),
      radius = 3.0,
      color = Some(Internal.Color.Red)
    )
    "translate External.Circle transformTo Internal.Circle" in {
      externalCircle.changeTo[Internal.Circle] shouldBe Right(internalCircle)
    }
    "translate Internal.Circle transformTo External.Circle" in {
      internalCircle.changeTo[External.Circle] shouldBe Right(externalCircle)
    }

    val externalPost = External.Post(
      author = "Misty",
      body = "#Fluent is a cool library to implement your #DDD #translationLayer seamlessly",
      timestamp = 1491823712002L
    )
    val externalPostWithOptionalFields = External.PostWithOptionalFields(
      author = Some("Misty"),
      body = Some("#Fluent is a cool library to implement your #DDD #translationLayer seamlessly"),
      timestamp = Some(1491823712002L)
    )
    val internalPost = Internal.Post(
      author = Internal.Author("Misty"),
      body = "#Fluent is a cool library to implement your #DDD #translationLayer seamlessly",
      timestamp = Instant.ofEpochMilli(1491823712002L),
      tags = List("#Fluent", "#DDD", "#translationLayer")
    )
    "transform External.Post to Internal.Post without extracting tags" in {
      implicit def toInstant(timestamp: Long): Instant = Instant.ofEpochMilli(timestamp)
      externalPost.transformTo[Internal.Post] shouldBe internalPost.copy(tags = Nil)
    }
    "transform External.PostWithOptionalFields to Internal.Post without extracting tags" in {
      implicit def toInstant(timestamp: Long): Instant = Instant.ofEpochMilli(timestamp)
      externalPostWithOptionalFields.transformTo[Internal.Post] shouldBe internalPost.copy(tags = Nil)
    }
    "transform External.Post to Internal.Post using user defined functions" in {
      implicit def tagsExtractor(post: External.Post): List[String] =
        post.body.split("\\s").toList.filter(_.startsWith("#"))
      implicit def toInstant(timestamp: Long): Instant = Instant.ofEpochMilli(timestamp)
      externalPost.transformTo[Internal.Post] shouldBe internalPost
    }
    "transform Internal.Post to External.Post using user defined functions" in {
      implicit def toTimestamp(instant: Instant): Long = instant.toEpochMilli
      internalPost.transformTo[External.Post] shouldBe externalPost
    }
    "transform Internal.Post to External.PostWithOptionalFields using user defined functions" in {
      implicit def toTimestamp(instant: Instant): Long = instant.toEpochMilli
      internalPost.transformTo[External.PostWithOptionalFields] shouldBe externalPostWithOptionalFields
    }
    "transform Internal.Published event to External.PostWithOptionalFields" in {
      implicit def toTimestamp(instant: Instant): Long = instant.toEpochMilli
      Internal.Published(internalPost).transformTo[External.PostWithOptionalFields] shouldBe externalPostWithOptionalFields
    }
    "transform Internal.Cylinder transformTo External.Circle" in {
      val cylinder = Internal.Cylinder(
        origin = Internal.Point(1.0, 2.0),
        radius = 3.0,
        height = 4.0,
        color = Internal.Color.Red
      )
      cylinder.transformTo[External.Circle] shouldBe externalCircle
    }
    "failed to transform when missing required field" in {
      case class Optional(value: Option[String])
      case class Required(value: String)
      Optional(None).changeTo[Required] shouldBe Left(TransformError("Missing required field"))
      an [IllegalArgumentException] should be thrownBy Optional(None).transformTo[Required]
    }
    "failed to transform string into case object if name doesn't match" in {
      case object SomethingElse
      "Something".changeTo[SomethingElse.type] shouldBe Left(TransformError("Can't transform 'Something' into SomethingElse"))
      an [IllegalArgumentException] should be thrownBy "Something".transformTo[SomethingElse.type]
    }
  }

}
