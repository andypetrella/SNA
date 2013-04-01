package controllers

import scala.concurrent.Future

import play.api._
import play.api.mvc._

import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.json.util._
import play.api.libs.functional.syntax._

import be.nextlab.play.neo4j.rest._
import be.nextlab.play.neo4j.rest.Neo4JElement._
import be.nextlab.play.neo4j.rest.Node._



object Application extends Controller {
  import store.Neo4J._

  def elementId(self:String) = self.substring(self.lastIndexOf('/') + 1).toInt
  val pickId       = (__ \ 'self).json.pick
  val pickData     = (__ \ 'data).json.pick
  val pickName     = (__ \ 'data \ 'name).json.pick
  val nodeReducer  = pickId and pickData reduce
  val graphReducer = pickId and pickName reduce

  def randomItems[A](s:Seq[A], nb:Int) = scala.util.Random.shuffle(s).take(nb)


  def index = Action { implicit  request =>
    Ok(views.html.index("Your new application is ready."))
  }

  implicit class QuickRel(n:Node) {
    case class H(r:String) {
      def ->(t:Node) = (m:Map[String, JsValue]) => Relation(Right(n), Right(t), r, Nil,m.toList:_*)
    }
    def -(r:String) = H(r)
  }


  def graphNodesAndLinks(name:String)(root:Root, ref:Node) = {
    for {
      graph <-  retrieveGraph(name)(ref)
      nodes <-  root.cypher(Cypher(
                  s"""
                    START g=node(${graph.id})
                    MATCH g-[:NODE]-n-[?:LINK]-l
                    return n, collect(ID(l)) as ls
                  """
                ))
    } yield nodes
  }
  def graphNodesAndLinksAsJson(name:String)(root:Root, ref:Node) = {
    graphNodesAndLinks(name)(root, ref).map { cr =>
      val json = JsArray(cr.resultAsMap.map { nodeAndLinksMap =>
        val node = nodeAndLinksMap("n")
                    .transform(nodeReducer)
                    .recoverTotal{
                      case jserror => JsError.toFlatJson(jserror)
                    } //BANG
        val links = nodeAndLinksMap("ls")
        Json.obj(
          "node" -> node,
          "links" -> links
        )
      })
      json
    }
  }
  def retrieveGraph(name:String) = (ref:Node) =>
    for {
      graph  <-  ref.linkedBy("GRAPH", Relation.Out)
                    .map{_.filter(g => (g \ "data" \ "name").asOpt[String] == Some(name))}
                    .map{_.headOption} if graph.isDefined
    } yield Node(graph.get)

  def newGraph(name:String, avgDegree:Double, infectionRate:Double) = (root:Root, ref:Node) =>
    for {
      graphCy <- root.cypher(Cypher(s"""
                      START r=node(${ref.id})
                      CREATE
                        p1=r-[:GRAPH]->(g {name: "$name" , `avg-degree`: $avgDegree , infectionRate: $infectionRate})
                      RETURN g
                    """))
    } yield graphCy("g")
              .collect({case x:JsObject => x})
              .map(g => Node(g))
                .headOption
                .getOrElse(throw new IllegalStateException("Unable to create the graph"))

  def createGraph(name:String, avgDegree:Double, infectionRate:Double) = Action { implicit request =>
    Async {
      val graph = for {
        root  <- endpoint.root
        ref   <- root.referenceNode
        graph <- newGraph(name, avgDegree,infectionRate)(root, ref)
      } yield graph
      graph.map { g =>
        Ok("Graph created")
      }.recover {
        case t => BadRequest(t.getMessage)
      }
    }
  }
  def generateRandomGraph(name:String, degree:Double, numberOfNodes:Int, infectionRate:Double) = Action {
    Async {
      (
        for {
          root  <- endpoint.root
          ref   <- root.referenceNode
          graph <- newGraph(name, degree, infectionRate)(root, ref)
          nodes <- Future.sequence((1 to numberOfNodes).map{_ => newNode(graph)(root)})
          _     <- nodes.foldLeft(Future.successful(())) { (f, node) =>
                    f.flatMap(xs => createLinks(node)(root).map(_ => ()))
                  }
        } yield Ok(name)
      ).recover {
        case t => {
          t.printStackTrace
          BadRequest(t.getMessage)
        }
      }
    }
  }
  def graphExplorer = Action { implicit request =>
    Async {
      val graphsFuture = for {
        root  <-  endpoint.root
        ref   <-  root.referenceNode
        graphs <-  ref.linkedBy("GRAPH", Relation.Out)
      } yield {
        graphs
          .view
          .map(x => Node(x))
          .map{x => (x.id, (x.data \ "name").asOpt[String])}
          .collect {
            case (id, Some(name)) => (id, name)
          }
      }
      graphsFuture.map{ gs =>
        Ok(views.html.explorer(gs))
      }
    }
  }
  def getGraph(name:String) = Action { implicit request =>
    Async {
      for {
        root  <-  endpoint.root
        ref   <-  root.referenceNode
        g     <- graphNodesAndLinksAsJson(name)(root,ref)
                  .map{x => Ok(x)}
                  .recover{
                    case x => {
                      x.printStackTrace
                      BadRequest(x.getMessage)
                    }
                  }

      } yield g
    }
  }



  val infectionStrategy = (b:Boolean) => (node:JsValue) => (node \ "data" \ "infected").asOpt[Boolean] == Some(b)
  def putInfection(node:JsValue, heal:Boolean):Future[Node] = putInfection(Node(node.asInstanceOf[JsObject]))
  def putInfection(node:Node, heal:Boolean = false):Future[Node] = node <+ ("infected", Some(JsBoolean(!heal)))
  def newNode(graph:Node) = (root:Root) =>
    for {
      nodeCy <- root.cypher(Cypher(s"""
                      START g=node(${graph.id})
                      CREATE
                        p1=g-[:NODE]->(n {infected: false})
                      RETURN n
                    """))
      } yield nodeCy("n")
                .collect({case x:JsObject => x})
                .map(x => Node(x))
                  .headOption
                    .getOrElse(throw new IllegalStateException("Cannot create node"))

  def createLinks(node:Node) = (root:Root) =>
    for {
      (nodes, deg)  <- (root.cypher(Cypher(s"""
                              START n=node(${node.id})
                              MATCH n<-[:NODE]-g-[:NODE]->o
                              WHERE not(id(n) = id(o))
                              RETURN o, g.`avg-degree` as v
                            """))).map {cy =>
                              (
                                cy("o").collect({case x:JsObject => x}).map(x=>Node(x)),
                                cy("v").headOption.map{_.as[Double]}.getOrElse(throw new IllegalStateException("cannot get avg degree"))
                              )
                            }
      ts            <-  {
                          val nb = math.ceil(deg / 2).toInt
                          val targets = randomItems(nodes, nb)
                          val ns=targets.zipWithIndex.map{x =>
                            val alias = s"t${x._2}"
                            (s"$alias=node(${x._1.id})", s"p_$alias = n-[:LINK]->$alias", alias)
                          }
                          val prep = ns.reduce( (x,y) => (x._1+","+y._1, x._2+",\n"+y._2, x._3+","+y._3))
                          root.cypher(Cypher(s"""
                                              |START n=node(${node.id}),${prep._1}
                                              |CREATE
                                              |  ${prep._2}
                                              |RETURN
                                              |  ${prep._3}
                                            """.stripMargin('|')))
                              .map { cy =>
                                ns.view.map{_._3}.map { t =>
                                  cy(t).collect({case x:JsObject => x}).headOption.map{x=>Node(x)}
                                }.collect({case Some(x)=>x})
                              }
                        }
      } yield ts

  def createNode(graphName:String) = Action { implicit request =>
    Async {
      val json = for {
        root    <- endpoint.root
        ref     <- root.referenceNode
        graph   <- retrieveGraph(graphName)(ref)
        node    <- newNode(graph)(root)
        targets <- createLinks(node)(root)
      } yield {
          val n:JsValue = node.jsValue.transform(nodeReducer).recoverTotal({jserror => JsError.toFlatJson(jserror)})
          Json.obj(
            "node"  -> n, //BANG
            "links" -> {
              JsArray(targets.map{x=>JsNumber(x.id)})
            }
          )
        }

      json.map { j =>
        Ok(j)
      }.recover {
        case t => {
          t.printStackTrace
          BadRequest(t.getMessage)
        }
      }
    }
  }


  def infection(graphName:String) = Action { implicit request =>
    Async {
      (
        for {
          root  <-  endpoint.root
          ref   <-  root.referenceNode
          nodesNLinks <-  graphNodesAndLinks(graphName)(root, ref)
          node        <-  nodesNLinks("n").filter(infectionStrategy(false)) match {
                            case x if x.length==0 => Future.successful(None)
                            case nodes => putInfection(nodes(scala.util.Random.nextInt(nodes.length)), true).map { x => Some(x)}
                          } if node.isDefined
        } yield {
          val n:JsValue = node.get.jsValue.transform(nodeReducer).recoverTotal(jserror => JsError.toFlatJson(jserror))
          Ok(Json.obj("node" -> n, "links" -> JsNull))
        }
      ) recover {
        case x => {
          x.printStackTrace
          BadRequest(x.getMessage)
        }
      }
    }
  }

  def expand(graphName:String, heal:Boolean = false) = Action { implicit request =>
    Async {
      (
        for {
          root            <-  endpoint.root
          ref             <-  root.referenceNode
          (rate, yes, no) <- root.cypher(Cypher(s"""
                                  START n=node(*)
                                  MATCH n-[:GRAPH]->g-[:NODE]->i-[:LINK]->t
                                  WHERE
                                    g.name! = '$graphName' AND
                                    i.infected! = true AND
                                    t.infected! = false
                                  RETURN i, g.infectionRate as r, t;
                                """))
                              .map{ cy =>
                                (
                                  cy("r").headOption.map{_.as[Double]}.getOrElse(0d),
                                  cy("i").collect({case x:JsObject => x}).map(x => Node(x)),
                                  cy("t").collect({case x:JsObject => x}).map(x => Node(x))
                                )
                              }
          infected        <-  Future.sequence(
                                no.filter(_ => scala.util.Random.nextFloat <= rate)
                                  .distinct
                                  .map {x => putInfection(x)}
                              )
          noMore          <-  if (!heal)
                                Future.successful(Nil)
                              else
                                Future.sequence(
                                  yes.filter(_ => scala.util.Random.nextFloat <= rate)
                                    .distinct
                                    .map {x => putInfection(x, true)}
                                )
        } yield {
          Ok(
            JsArray(
              (infected ++ noMore).map { x =>
                val n:JsValue = x.jsValue.transform(nodeReducer).recoverTotal(jserror => JsError.toFlatJson(jserror))
                Json.obj("node" -> n, "links" -> JsNull)
              }.distinct
            )
          )
        }
      ) recover {
        case x => {
          x.printStackTrace
          BadRequest(x.getMessage)
        }
      }
    }
  }


}