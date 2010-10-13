/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package GameData

import dip.world._

class MovesWithoutConvoy (w : World, pName : String) {
      val m : List[(Location, List[Location])] = movesConstructor

      private type LocPair = (Location, Location)
      private type PairList = List[LocPair]
      private def movesConstructor : List[(Location, List[Location])] = {
          val tState : TurnState = w.getLastTurnState()
          val cPos : Position = tState.getPosition()
          val m : Map = w.getMap()
          val p : Power = m.getClosestPower(pName)
          val provs : List[Province] = cPos.getUnitProvinces(p).toList
          val units : List[Unit] = provs map (cPos.getUnit)
          val hlocs : List[Location]
                    = (provs zip units) map 
                              (u => new Location(u._1, u._2.getCoast))
          val alocs : List[List[Location]]
                    = (provs zip units) map (u =>
                                u._1.getAdjacentLocations(u._2.getCoast).toList)
          hlocs zip alocs
      }
      
      def generateSrcDstPairs : PairList = {
            val l = m map (u => u._2 map (v => (u._1, v)))
            val compressList : (PairList, PairList) => PairList =
                                                              (p, q) => p ++ q
            l.foldLeft(Nil : PairList)(compressList)
      }

      def getMoves(l : Location) : List[Location] =
        (m find (u => u._1.equals(l))) match {
          case Some(x) => x._2
          case None => Nil
        }
}
