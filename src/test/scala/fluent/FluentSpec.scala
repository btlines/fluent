package fluent

import java.time.Instant

import org.scalatest.{ Matchers, WordSpecLike }

object External {
  case class Circle(x: Double, y: Double, radius: Double, color: Option[String])
  case class Post(author: String, body: String, timestamp: Long)
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

  case class Author(name: String)
  case class Post(author: Author, body: String, tags: List[String], timestamp: Instant)
}

class FluentSpec extends WordSpecLike with Matchers {

  // Needed to support transformation on Functors (Option, List, ...)
  import cats.instances.all._

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
    "translate External.Circle into Internal.Circle" in {
      externalCircle.transformTo[Internal.Circle] shouldBe internalCircle
    }
    "translate Internal.Circle into External.Circle" in {
      internalCircle.transformTo[External.Circle] shouldBe externalCircle
    }

    val externalPost = External.Post(
      author = "Misty",
      body = "#Fluent is a cool library to implement your #DDD #translationLayer seamlessly",
      timestamp = 1491823712002L
    )
    val internalPost = Internal.Post(
      author = Internal.Author(name = "Misty"),
      body = "#Fluent is a cool library to implement your #DDD #translationLayer seamlessly",
      timestamp = Instant.ofEpochMilli(1491823712002L),
      tags = List("#Fluent", "#DDD", "#translationLayer")
    )
    "translate External.Post to Internal.Post using user defined functions" in {
      implicit def tagsExtractor(post: External.Post): List[String] =
        post.body.split("\\s").toList.filter(_.startsWith("#"))
      implicit def toInstant(timestamp: Long): Instant = Instant.ofEpochMilli(timestamp)
      externalPost.transformTo[Internal.Post] shouldBe internalPost
    }
    "translate Internal.Post to External.Post using user defined functions" in {
      implicit def toTimestamp(instant: Instant): Long = instant.toEpochMilli
      internalPost.transformTo[External.Post] shouldBe externalPost
    }
  }

}
