package com.bigdlittled.andes

import scalaz._
import scalax.collection.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
import scalax.collection.edge.LDiEdge     // labeled directed edge
import scalax.collection.edge.Implicits._ // shortcuts
import scala.collection.mutable.ArrayBuffer
import domain._

object Main extends App {
 import Permission._
 import Taxonomy._
 import Filters._

 val g = Graph(
   Holding(Investment("Global 60/40", Portfolio), Investment("Equities"), 0.5),
     Holding(Investment("Equities"), Investment("US Equities"), 0.6),
       Holding(Investment("US Equities"), Investment("US Equities Excess Returns", ReturnStream), 1.0),
       Holding(Investment("US Equities"), Investment("USD Cash Returns", ReturnStream), 1.0),
     Holding(Investment("Equities"), Investment("EUR Equities"), 0.4),
       Holding(Investment("EUR Equities"), Investment("EUR Equities Excess Returns", ReturnStream), 1.0),
       Holding(Investment("EUR Equities"), Investment("EUR Cash Returns", ReturnStream), 1.0),
   Holding(Investment("Global 60/40", Portfolio), Investment("Bonds"), 0.5),
     Holding(Investment("Bonds"), Investment("Nominal Bonds"), 0.7),
       Holding(Investment("Nominal Bonds"), Investment("Nominal Bonds Excess Returns", ReturnStream), 1.0),
       Holding(Investment("Nominal Bonds"), Investment("USD Cash Returns", ReturnStream), 1.0),
     Holding(Investment("Bonds"), Investment("IL Bonds"), 0.3),
       Holding(Investment("IL Bonds"), Investment("IL Bonds Excess Returns", ReturnStream), 1.0),
       Holding(Investment("IL Bonds"), Investment("USD Cash Returns", ReturnStream), 1.0),
   Permission(Investment("Global 60/40", Portfolio), User("Big Boss"), Read)
 )

  // All the nodes
  println(g.nodes mkString ":")
 
  // All the holdings
  println(g.edges mkString ":")

  // The top level portfolio
  val p = g get Investment("Global 60/40", Portfolio)
 
  // All the nodes with a traverser 
  println(p.outerNodeTraverser.map(_.toString()))

  // Only the assets
  println(p.outerNodeTraverser.filter(AssetsOnly).map(_.toString()))
 
  // Only the return streams
  println(p.outerNodeTraverser.filter(ReturnStreamsOnly).map(_.toString()))

  // Only the portfolios
  println(p.outerNodeTraverser.filter(PortfoliosOnly).map(_.toString()))

  // Same thing with a for comprehension
  println(
    for {
      a <- p.outerNodeTraverser
      if PortfoliosOnly(a)
    } yield a.toString()
  )

  // Only the users
  println(p.outerNodeTraverser.filter(UsersOnly).map(_.toString()))

  val u = g get User("Big Boss")
 
  // Find all the portfolios that a user has some kind of access to
  println(u.diPredecessors.map(_.toString()))
 
  // And with a for comprehension
  // TODO: Figure out why the Set can't be filtered with filter(PortfoliosOnly)
  println(
    for {
      p <- g get User("Big Boss") diPredecessors;
      if PortfoliosOnly(p)
    } yield p.toString()
  )
  
  // Same thing with a one way traverser
  println((ArrayBuffer.empty[String] /: p.innerNodeTraverser)(_ += _.toString()).mkString)
 
  // And with an up down traverser
  println((ArrayBuffer.empty[String] /: p.innerNodeDownUpTraverser) {
    (buf, param) => param match {
      case (down, node) => 
        if (down) buf += (if (node eq p) "(" else "[") += node.toString() // Going down...
        else      buf += (if (node eq p) ")" else "]")                    // ...and up
    }
  }.mkString)

}