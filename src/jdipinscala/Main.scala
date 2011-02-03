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

  def getMovements(playerName : String) : JSONArray = {
    val powerName = Constants.nameToCountryTable.get(playerName)
    val fullGameName = String.format("%s/%s.jdip", Constants.savePath, Constants.gameName)
    val f : File = new File(fullGameName)
    val world : World = World.open(f)
    val ts : TurnState = world.getLastTurnState
    val position : Position = ts.getPosition
    val worldMap : dip.world.Map = world.getMap
    val playerPower : dip.world.Power = worldMap.getPowerMatching(powerName)

    JSONArray(position.getUnitProvinces(playerPower).toList map ((p : Province) => {
        val u : dip.world.Unit = position.getUnit(p)
        JSONObject(Map("unitType" -> u.getType.toString, "provinceName" -> p.getFullName))
      }))

  }

  /**
   * @param args the command line arguments
   */
  def main(args: Array[String]) : Unit = {
    val hostName : String = Constants.hostAddress

    println("Starting server")
    
    val gameServerActor = Actor.actor {
      RemoteActor.alive(Constants.hostPort)
      RemoteActor.register(Symbol("myname"), Actor.self)

      def waitForMessage : Unit = Actor.react {
        case Some((x : String, "getMovements")) =>
          val movements = getMovements(x)
          Actor.sender ! movements
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
