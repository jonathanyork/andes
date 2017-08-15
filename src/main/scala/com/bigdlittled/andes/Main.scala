package com.bigdlittled.andes

import scalaz._
import scalax.collection.Graph // or scalax.collection.mutable.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
import scalax.collection.edge.LDiEdge     // labeled directed edge
import scalax.collection.edge.Implicits._ // shortcuts

object Main extends App {
 import Holding._
 
 val g = Graph(
   Investment("Portfolio")~>Investment("Equities") ### 0.5,
   Investment("Portfolio")~>Investment("Bonds") ### 0.5)
      
 g.nodes.foreach(n => println("Node = '" + n + "'"))

 val p = g get Investment("Portfolio")
 
 show(p)
 
 def show(investment: Investment): Unit = {
   println(p)
   (p diSuccessors).foreach(show(_))
 }
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