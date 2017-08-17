package com.bigdlittled.andes

import scalaz._
import scalax.collection.Graph // or scalax.collection.mutable.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
import scalax.collection.edge.LDiEdge     // labeled directed edge
import scalax.collection.edge.Implicits._ // shortcuts
import scala.collection.mutable.ArrayBuffer

object Main extends App {
 import Holding._
 
 val g = Graph(
   Investment("Portfolio")~>Investment("Equities") ### 0.5,
     Investment("Equities")~>Investment("US Equities") ### 0.6,
     Investment("Equities")~>Investment("EUR Equities") ### 0.4,
   Investment("Portfolio")~>Investment("Bonds") ### 0.5,
     Investment("Bonds")~>Investment("Nominal Bonds") ### 0.7,
     Investment("Bonds")~>Investment("IL Bonds") ### 0.3)
      
 g.nodes.foreach(n => println("Node = '" + n + "'"))

 val p = g get Investment("Portfolio")
 
 val (n0, n1, n2) = (g get Investment("Portfolio"), g get Investment("Equities"), g get Investment("Bonds"))
 
 val result = (ArrayBuffer.empty[String] /: p.innerNodeDownUpTraverser) {
   (buf, param) => param match {
     case (down, node) => 
       if (down) buf += (if (node eq p) "(" else "[") += node.toString
       else      buf += (if (node eq p) ")" else "]")
   }
 }
 
 println(result)
 
}

case class Investment(name: String)

case class User(name: String)

case class Holding[+N](portfolio: N, asset: N, amount: Double)
  extends DiEdge[N](NodeProduct(portfolio, asset))
  with    ExtendedKey[N]
  with    EdgeCopy[Holding]
  with    OuterEdge[N,Holding] 
{
  private def this(nodes: Product, amount: Double) {
    this(nodes.productElement(0).asInstanceOf[N],
         nodes.productElement(1).asInstanceOf[N], amount)
  }
  def keyAttributes = Seq(amount)
  override def copy[NN](newNodes: Product) = new Holding[NN](newNodes, amount)
  override protected def attributesToString = s" ($amount)" 
}
object Holding {
  implicit final class ImplicitEdge[A <: Investment](val e: DiEdge[A]) extends AnyVal {
    def ### (amount: Double) = new Holding[A](e.source, e.target, amount)
  } 
}