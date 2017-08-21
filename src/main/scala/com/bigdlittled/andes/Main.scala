package com.bigdlittled.andes

import scalaz._
import scalax.collection.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
import scalax.collection.edge.LDiEdge     // labeled directed edge
import scalax.collection.edge.Implicits._ // shortcuts
import scala.collection.mutable.ArrayBuffer
import domain._

object Main extends App {
 import Holding._
 import Taxonomy._
 import Filters._

 val g = Graph(
   Investment("Global 60/40", Portfolio)~>Investment("Equities") ### 0.5,
     Investment("Equities")~>Investment("US Equities") ### 0.6,
       Investment("US Equities")~>Investment("US Equities Excess Returns", ReturnStream) ### 1.0,
       Investment("US Equities")~>Investment("USD Cash Returns", ReturnStream) ### 1.0,
     Investment("Equities")~>Investment("EUR Equities") ### 0.4,
       Investment("EUR Equities")~>Investment("EUR Equities Excess Returns", ReturnStream) ### 1.0,
       Investment("EUR Equities")~>Investment("EUR Cash Returns", ReturnStream) ### 1.0,
   Investment("Global 60/40", Portfolio)~>Investment("Bonds") ### 0.5,
     Investment("Bonds")~>Investment("Nominal Bonds") ### 0.7,
       Investment("Nominal Bonds")~>Investment("Nominal Bonds Excess Returns", ReturnStream) ### 1.0,
       Investment("Nominal Bonds")~>Investment("USD Cash Returns", ReturnStream) ### 1.0,
     Investment("Bonds")~>Investment("IL Bonds") ### 0.3,
       Investment("IL Bonds")~>Investment("IL Bonds Excess Returns", ReturnStream) ### 1.0,
       Investment("IL Bonds")~>Investment("USD Cash Returns", ReturnStream) ### 1.0,
   (Investment("Global 60/40", Portfolio)~+#>User("Big Boss"))(Permission.Read))

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

  // Same thing with a one way traverser
  println((ArrayBuffer.empty[String] /: p.innerNodeTraverser)(_ += _.name).mkString)
 
  // And with an up down traverser
  println((ArrayBuffer.empty[String] /: p.innerNodeDownUpTraverser) {
    (buf, param) => param match {
      case (down, node) => 
        if (down) buf += (if (node eq p) "(" else "[") += node.name // Going down...
        else      buf += (if (node eq p) ")" else "]")              // ...and up
    }
  }.mkString)

}