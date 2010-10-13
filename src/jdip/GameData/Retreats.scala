/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package GameData

import dip.world._
import dip.order._
import dip.gui.order.GUIOrderFactory

class Retreats (w : World, pName : String) {
  private def retreatConstructor : List[(Location, List[Location])] = {
    val tState : TurnState = w.getLastTurnState
    val cPos : Position = tState.getPosition
    val m : Map = w.getMap
    val p : Power = m.getClosestPower(pName)
    val dp : List[Province] = cPos.getDislodgedUnitProvinces(p).toList
    val du : List[Unit] = dp map (cPos.getDislodgedUnit)
    val dl : List[Location] = (dp zip du) map
                                        (u=> new Location(u._1, u._2.getCoast))
    val aLoc : List[List[Location]] = (dp zip du) map
              (u => u._1.getAdjacentLocations(u._2.getCoast).toList
                     filter (v => !cPos.hasUnit(v.getProvince) &&
                                  !wasInvaded(tState, v) &&
                                  !wasOccupiedByInvader(tState, v,
                                            new Location(u._1, u._2.getCoast))))
    dl zip aLoc
  }

  val retreats : List[(Location, List[Location])] = retreatConstructor

  private def wasOccupiedByInvader(t : TurnState, src : Location,
                                                  dst : Location) : Boolean = {
    val pTS : TurnState = w.getPreviousTurnState(t)
    val m : Map = w.getMap
    val p : Power = m.getClosestPower(pName)
    val g : GUIOrderFactory = new GUIOrderFactory
    val ords : java.util.List[_] = pTS.getAllOrders
    val ordsI : List[Orderable] = List.range(0, ords.size) map
                                      (u => ords.get(u).asInstanceOf[Orderable])
    val orders : List[Move] = ordsI filter (u => u.getFullName == "Move") map
                                    (u => u.asInstanceOf[Move]) filter
                                    pTS.isOrderSuccessful
    orders exists (u => u.getSource.isProvinceEqual(src) &&
                          u.getDest.isProvinceEqual(dst))
  }

  private def wasInvaded(t : TurnState, l : Location) : Boolean = {
    val pTS : TurnState = w.getPreviousTurnState(t)
    val m : Map = w.getMap
    val p : Power = m.getClosestPower(pName)
    val g : GUIOrderFactory = new GUIOrderFactory
    val ords : java.util.List[_] = pTS.getAllOrders
    val ordsI : List[Orderable] = List.range(0, ords.size()) map
                                      (u => ords.get(u).asInstanceOf[Orderable])
    val orders : List[Move] = ordsI  filter (u => u.getFullName == "Move") map
                                                     (u => u.asInstanceOf[Move])
    orders exists (u => u.getDest.isProvinceEqual(l))
  }

}
