/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package jdipinscala

import jdip.imageOutput.ImageCreator

import java.util.Properties
import java.io.File
import java.io.FileInputStream
import dip.gui.ClientFrame
import GameData._
import GameController._
import java.net.ServerSocket
import java.net.Socket
import java.io.ObjectInputStream
import scala.actors.remote.RemoteActor

import scala.collection.JavaConversions._

import dip.world.{Unit => _, _}
import dip.gui.order2._
import dip.order.Move

object Main {

  /**
   * @param args the command line arguments
   */
  def main(args: Array[String]) : Unit = {
    val props : Properties = new Properties;
    props.load(new FileInputStream(args(0) + "jDipConfiguration.properties"))
    val savepath : String = props.getProperty("savepath")
    val imagepath : String = props.getProperty("imagepath")
    val gameName : String = props.getProperty("gameName")
    val pName : String = props.getProperty("pName")
    val variantPath : String = props.getProperty("variantPath")
    val fullGameName : String = String.format("%s%s", savepath, gameName)
    val f : File = new File(fullGameName);

    val world : World = World.open(f);
    val ts : TurnState = world.getLastTurnState
    val position : Position = ts.getPosition
    val worldMap : dip.world.Map = world.getMap
    val francePower : Power = worldMap.getPowerMatching(pName);

    val gOrderFactory : GUIOrderFactory = new GUIOrderFactory

    val srcProvince : Province = worldMap.getProvinceMatching("mar")
    val srcUnit : dip.world.Unit = position.getUnit(srcProvince)
    val srcLocation : dip.world.Location = 
      new Location(srcProvince, srcUnit.getCoast)
    val destProvince : Province = worldMap.getProvinceMatching("bur")
    val destLocation : dip.world.Location =
      new Location(destProvince, Coast.NONE)

    /*setting some moves for France and resolving*/
    val mv1 : Move =
      gOrderFactory.createMove(francePower,
                               srcLocation,
                               srcUnit.getType,
                               destLocation)

    val srcProvince2 : Province = worldMap.getProvinceMatching("par")
    val srcUnit2 : dip.world.Unit = position.getUnit(srcProvince2)
    val srcLocation2 : dip.world.Location =
      new Location(srcProvince2, srcUnit2.getCoast)
    val destProvince2 : Province = worldMap.getProvinceMatching("pic")
    val destLocation2 : dip.world.Location =
      new Location(destProvince2, Coast.NONE)

    val mv2 : Move = gOrderFactory.createMove(francePower, srcLocation2, srcUnit2.getType, destLocation2)

    val srcProvince3 : Province = worldMap.getProvinceMatching("bre")
    val srcUnit3 : dip.world.Unit = position.getUnit(srcProvince3)
    val srcLocation3 : dip.world.Location = new Location(srcProvince3, srcUnit3.getCoast)
    val destProvince3 : Province = worldMap.getProvinceMatching("eng")
    val destLocation3 : dip.world.Location = new Location(destProvince3, Coast.SEA)
    val mv3 : Move = gOrderFactory.createMove(francePower, srcLocation3, srcUnit3.getType, destLocation3)

    val orderList : List[Move] = mv1 :: mv2 :: mv3 :: Nil

    val orderListJava : java.util.List[Move] = orderList

    ts.setOrders(francePower, orderListJava)

    val imgCreator : ImageCreator = new ImageCreator(savepath, variantPath, false)
    
    imgCreator.process("trialWorld", world)

    println(ts)
  }

}
