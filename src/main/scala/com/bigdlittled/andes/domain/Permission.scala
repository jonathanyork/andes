package com.bigdlittled.andes.domain

import scalaz._
import scalax.collection.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
import scalax.collection.edge.LDiEdge     // labeled directed edge
import scalax.collection.edge.Implicits._ // shortcuts

case class Permission[+N](investment: N, user: N, operation: Permission.Operation)
  extends DiEdge[N](NodeProduct(investment, user))
  with    ExtendedKey[N]
  with    EdgeCopy[Permission]
  with    OuterEdge[N,Permission] {

  private def this(nodes: Product, operation: Permission.Operation) {
    this(nodes.productElement(0).asInstanceOf[N],
         nodes.productElement(1).asInstanceOf[N], operation)
  }

  def keyAttributes = Seq(operation)
  override def copy[NN](newNodes: Product) = new Permission[NN](newNodes, operation)
  override protected def attributesToString = s" ($operation)" 
}

object Permission {
  sealed trait Operation
  case object Read extends Operation
  case object Modify extends Operation
  case object Delete extends Operation
  case object Own extends Operation
  val operations = Seq(Read, Modify, Delete, Own)
}