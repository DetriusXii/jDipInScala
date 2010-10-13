/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package GameData

import dip.world._

class Builds(w : World, pName : String) {
  private val tState : TurnState = w.getLastTurnState
  private val cPos : Position = tState.getPosition
  private val m : Map = w.getMap
  private val p : Power = m.getClosestPower(pName)
  private val ownedProvs : List[Province] = cPos.getOwnedSupplyCenters(p).toList
  private val unitProvs : List[Province] = cPos.getUnitProvinces(p).toList

  val netGain : Int = ownedProvs.length - unitProvs.length
  val bLocs : Option[List[Location]] =
    if (netGain <= 0)
      None
    else
      new Some(buildLocations)

  val rLocs : Option[List[Location]] =
    if (netGain >= 0)
      None
    else
      new Some(removeLocations)

  private def buildLocations : List[Location] = {
    val homeProv : List[Province] = cPos.getHomeSupplyCenters(p).toList filter
                                      (u => !cPos.hasUnit(u))
    val bLocs : List[Location] = homeProv
                  .foldLeft(Nil : List[Location])((u,v) => {
                      val landLoc : Location = new Location(v, Coast.LAND)
                      val q : List[Location] =
                        if (v.isMultiCoastal) {
                          val c : List[Coast]
                                            = v.getValidDirectionalCoasts.toList
                          val coastLocs : List[Location] =
                                                 c map (u => new Location(v, u))
                          landLoc::coastLocs
                        } else if (v.isCoastal) {
                          val coastLoc : Location = new Location(v, Coast.SEA)
                          landLoc::coastLoc::Nil
                        } else {
                          landLoc::Nil
                        }
                      u:::q
                 })
    bLocs
  }

  private def removeLocations : List[Location] = {
    val provs : List[Province] = cPos.getUnitProvinces(p).toList
    val units : List[Unit] = provs map cPos.getUnit
    (provs zip units) map (u => new Location(u._1, u._2.getCoast))
  }
}
