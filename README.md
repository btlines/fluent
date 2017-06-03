[![Build status](https://api.travis-ci.org/btlines/fluent.svg?branch=master)](https://travis-ci.org/btlines/fluent)
[![codecov](https://codecov.io/gh/btlines/fluent/branch/master/graph/badge.svg)](https://codecov.io/gh/btlines/fluent)
[![Dependencies](https://app.updateimpact.com/badge/852442212779298816/fluent.svg?config=compile)](https://app.updateimpact.com/latest/852442212779298816/fluent)
[![License](https://img.shields.io/:license-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![Download](https://api.bintray.com/packages/beyondthelines/maven/fluent/images/download.svg) ](https://bintray.com/beyondthelines/maven/fluent/_latestVersion)

# Fluent
The seamless translation layer

## Context

In DDD (Domain Driven Design) it is recommended to introduce a translation layer (aka anticorruption layer) between different domains. It allows to isolate the domain from each other making sure the can evolve independently.

However writing code for the translation layer isn't really exciting as quite often the domain objects share some similarities. Translating from one domain to the other introduces lots of boilerplate code and doesn't add much business value.

Fluent aims at reducing this boilerplate code. It leverages the similarities between the domains in order to convert case classes from one domain into the other. Under the hood it relies on Shapeless and implicit type class instances.

## Setup

In order to use Fluent you need to add the following lines to your `build.sbt`:

```scala
resolvers += Resolver.bintrayRepo("beyondthelines", "maven")

libraryDependencies += "beyondthelines" %% "fluent" % "0.0.5"
```

## Dependencies

Fluent has only 2 dependencies: Shapeless and Cats.

## Usage

In order to use Fluent you need to import the following:

```scala
import cats.intances.all._ 
import fluent._
```

The cats import is only needed if your case classes contains functors like `List`, `Option`, ... Of course you can be more specific and import only the instances you need. 

Importing fluent._ adds a new method to your case classes `transformTo`. You only need to call this method to transform a case class into another one.

### Basic Example

Let's take a basic example to illustrate the usage:

```scala
object External {
  case class Circle(x: Double, y: Double, radius: Double, color: Option[String])
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
}
```

Let's create an instance of external circle:

```scala
val externalCircle = External.Circle(
  x = 1.0,
  y = 2.0,
  radius = 3.0,
  color = Some("Red")
)
```

and turn it into an internal circle

```scala
val internalCircle = externalCircle.transformTo[Internal.Circle]
```

In this case it's easy to figure out what Fluent does:
`External.Circle` contains a `x` and a `y` of type `Double` so Fluent can create a `Point` from an `External.Circle`. The `radius` can be taken as it is and `color` needs to be turned into the correct type (Note that if the colour doesn't exist in the expected values it would fail at runtime).

### Using user defined functions

Sometimes Fluent can figure out how to convert from one case class to the other. In such cases it's possible to define implicit functions that Fluent can use to achieve the conversion.

Let's take another example:

```scala
import java.time.Instant

object External {
  case class Post(author: String, body: String, timestamp: Long)
}
object Internal {
  case class Author(name: String)
  case class Post(author: Author, body: String, tags: List[String], timestamp: Instant)
}
```

Let's create an `External.Post` instance:

```scala
val externalPost = External.Post(
  author = "Misty",
  body = "#Fluent is a cool library to implement your #DDD #translationLayer seamlessly",
  timestamp = 1491823712002L
)
```

In this case it's not possible to transform the `External.Post` into an `Internal.Post` because Fluent can't figure out how to extract the tags from the post and how to convert the `timestamp` of type `Long` into a `timestamp` of type `Instant`.

We can define 2 implicit functions to perform these transformations:

```scala
implicit def tagsExtractor(post: External.Post): List[String] =
  post.body.split("\\s").toList.filter(_.startsWith("#"))

implicit def toInstant(timestamp: Long): Instant = 
  Instant.ofEpochMilli(timestamp)
```

With these 2 functions in scope we can now transform the `External.Post` into an `Internal.Post`

```scala
val internalPost = externalPost.transformTo[Internal.Post]
```

### More information

More implementation details can be found on this blog post: http://www.beyondthelines.net/programing/fluent-a-deep-dive-into-shapeless-and-implicit-resolution/ 
