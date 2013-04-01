package store

import be.nextlab.play.neo4j.rest.Neo4JEndPoint
import be.nextlab.play.neo4j.rest.Neo4JRestPlugin

import play.api.libs.concurrent.Execution.Implicits._

import play.api.Play.current

object Neo4J {
  implicit lazy val endpoint: Neo4JEndPoint = current
      .plugin[Neo4JRestPlugin]
        .map(_.neo4j)
        .getOrElse(
          throw new IllegalStateException("Unable to find plugin configuration for : Neo4JRestPlugin")
        )

}