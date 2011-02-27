/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package jdipinscala

import jdip.imageOutput.ImageCreator

import java.util.Properties
import java.io.File
import java.io.FileInputStream
import GameData._
import jdip.scala.constants.Constants

import scala.actors.Actor
import scala.actors.remote.RemoteActor
import scala.collection.JavaConversions._

import dip.world.{Unit => _, Map => _, _}
import dip.gui.order2._
import dip.order.Move
import scala.util.parsing.json.JSONArray
import scala.util.parsing.json.JSONObject

object Main {

  def getMovements(world : World)(playerName : String) : List[Map[String, String]] = {
    val powerName = Constants.nameToCountryTable.get(playerName) match {
      case Some(x) => x
      case None => "France"
    }

    val ts : TurnState = world.getLastTurnState
    val position : Position = ts.getPosition
    val worldMap : dip.world.Map = world.getMap
    val playerPower : dip.world.Power = worldMap.getPowerMatching(powerName)

    val mbc : MovesByConvoy = new MovesByConvoy(world, powerName)
    val mwc : MovesWithoutConvoy = new MovesWithoutConvoy(world, powerName)

    def generateMapping(u : (Location, Location)) : Map[String, String] = {
        val srcUnitType = position.getUnit(u._1.getProvince).getType
        val provinceName = u._1.toString
        val srcName = u._1.toString
        val dstName = u._2.toString
        Map(Constants.UNIT_TYPE -> srcUnitType.toString,
            Constants.PROVINCE_NAME -> provinceName,
            Constants.ORDER_TYPE -> "Move",
            Constants.SRC_NAME -> srcName,
            Constants.DST_NAME -> dstName)
    }

    val movesWithoutConvoyList : List[Map[String, String]] =
      mwc.generateSrcDstPairs map ( generateMapping )

    val movesWithConvoyList : List[Map[String, String]] =
      mbc.generateSrcDstPairs map ( generateMapping )

    val totalMovesList : List[Map[String, String]] =
      movesWithConvoyList.foldLeft(movesWithoutConvoyList)((u, v) => {
          if (u.contains(v)) {
            u
          } else {
            v :: u
          }
        })

    val sh : SupportHolds = new SupportHolds(world, powerName)
    val supportHoldsList : List[Map[String, String]] =
      sh.sH.foldLeft(Nil : List[Map[String, String]])((u, v) => {
        val srcUnitType = position.getUnit(v._1.getProvince).getType
        val provinceName = v._1.toString
        val srcName = v._1.toString
        val dstNames = v._2 map (_.toString)
        val dataList : List[Map[String, String]] = dstNames map ( (t : String) => {
            Map(Constants.UNIT_TYPE -> srcUnitType.toString,
                Constants.PROVINCE_NAME -> provinceName,
                Constants.ORDER_TYPE -> "SupportHold",
                Constants.SRC_NAME -> srcName,
                Constants.DST_NAME -> t)
        })
        dataList ::: u
      })

     val sm : SupportMoves = new SupportMoves(world, powerName)
     val supportMovesList : List[Map[String, String]] =
       sm.supportMoves.foldLeft(Nil : List[Map[String, String]])((u,v) => {
           val srcUnitType = position.getUnit(v._1.getProvince).getType
           val provinceName = v._1.toString
           val srcDstNames = v._2 map (t => {(t._1.toString, t._2.toString)})
           val dataList : List[Map[String, String]] = srcDstNames map (t => {
              Map(Constants.UNIT_TYPE -> srcUnitType.toString,
                  Constants.PROVINCE_NAME -> provinceName,
                  Constants.ORDER_TYPE -> "SupportMove",
                  Constants.SRC_NAME -> t._1,
                  Constants.DST_NAME -> t._2)
            })
           dataList ::: u
        })

     val convoys : Convoys = new Convoys(world, powerName)
     val convoysList : List[Map[String, String]] = 
       convoys.convoys.foldLeft(Nil : List[Map[String, String]])((u,v) => {
          val srcUnitType = position.getUnit(v._1.getProvince).getType
          val provinceName = v._1.toString
          val srcDstNames = v._2 map (t => {(t._1.toString, t._2.toString)})
          val dataList : List[Map[String, String]] = srcDstNames map (t => {
              Map(Constants.UNIT_TYPE -> srcUnitType.toString,
                  Constants.PROVINCE_NAME -> provinceName,
                  Constants.ORDER_TYPE -> "Convoy",
                  Constants.SRC_NAME -> t._1,
                  Constants.DST_NAME -> t._2)
            })
          dataList ::: u
        })
    
      totalMovesList ::: supportHoldsList ::: supportMovesList ::: convoysList
  }

  /**
   * @param args the command line arguments
   */
  def main(args: Array[String]) : Unit = {
    val hostName : String = Constants.hostAddress

    println("Starting server")

    val fullGameName = String.format("%s/%s.jdip", Constants.savePath, Constants.gameName)
    val f : File = new File(fullGameName)
    val world : World = World.open(f)
    
    val gameServerActor = Actor.actor {
      RemoteActor.alive(Constants.hostPort)
      RemoteActor.register(Symbol("myname"), Actor.self)

      def waitForMessage : Unit = Actor.react {
        case Some((x : String, "getMovements")) =>
          val movements = getMovements(world)(x)
          Actor.sender ! movements
          println("waitForMessage")
          waitForMessage
        case _ =>
          println("nonstandard game message")
          waitForMessage
      }

      println("Waiting for message")
      waitForMessage
    }
  }

}
