package com.bigdlittled.andes.domain

import scalaz._
import scalax.collection.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
import scalax.collection.edge.LDiEdge     // labeled directed edge
import scalax.collection.edge.Implicits._ // shortcuts
import scala.collection.mutable.ArrayBuffer

case class Investment(name: String, investmentType: Taxonomy.InvestmentType = Taxonomy.Asset)

case class Holding[+N](portfolio: N, asset: N, amount: Double)
  extends DiEdge[N](NodeProduct(portfolio, asset))
  with    ExtendedKey[N]
  with    EdgeCopy[Holding]
  with    OuterEdge[N,Holding] {

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

object Taxonomy {
  sealed trait InvestmentType
  case object Portfolio extends InvestmentType
  case object Asset extends InvestmentType
  case object ReturnStream extends InvestmentType
  val types = Seq(Portfolio, Asset, ReturnStream)
}

object Filters {
  case object AssetsOnly extends ((Investment) => Boolean) {
    def apply(i: Investment) = {i.investmentType == Taxonomy.Asset}
  }
  case object ReturnStreamsOnly extends ((Investment) => Boolean) {
    def apply(i: Investment) = {i.investmentType == Taxonomy.ReturnStream}
  }
  case object PortfoliosOnly extends ((Investment) => Boolean) {
    def apply(i: Investment) = {i.investmentType == Taxonomy.Portfolio}
  }
 }