/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package GameData

import dip.world._

class SupportHolds(w : World, pName : String) {
    val sH : List[(Location, List[Location])] = supportConstructor

    private def supportConstructor : List[(Location, List[Location])] = {
        val tState : TurnState = w.getLastTurnState
        val cPos : Position = tState.getPosition
        val m : Map = w.getMap
        val provs : List[Province] = cPos.getUnitProvinces.toList
        val mwocSelf : MovesWithoutConvoy = new MovesWithoutConvoy(w, pName)
        val supH : List[(Location, List[Location])] = mwocSelf.m map
                              (u => (u._1, u._2 filter 
                                     (v => provs exists
                                                (t => v.isProvinceEqual(t)))))
        supH
    }

  def getSupportHolds(l : Location) : List[Location] =
        (sH find (u => u._1.equals(l))) match {
          case Some(x) => x._2
          case None => Nil
        }
}
