/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package GameData

import dip.world._

class MovesByConvoy (w : World, pName : String) {
      val m : List[(Location, List[Location])] = movesConstructor

      private type LocPair = (Location, Location)
      private type PairList = List[LocPair]
      private def movesConstructor : List[(Location, List[Location])] = {
        val tState : TurnState = w.getLastTurnState
        val cPos : Position = tState.getPosition
        val m : Map = w.getMap
        val conPaths : Path = new Path(cPos)
        val provs : List[Province] = m.getProvinces.toList filter 
                                                    (v => v.isCoastal)
        val dstLocs : List[Location] = provs map
                                            (u => new Location(u, Coast.NONE))
        val p : Power = m.getClosestPower(pName)
        val lProvs : List[Province] = (cPos.getUnitProvinces(p).toList)
        val units : List[Unit] = lProvs map (cPos.getUnit)
        val srcLocs : List[Location] = (lProvs zip units) map
                                        (u => new Location(u._1, u._2.getCoast))
        val mwc : List[(Location, List[Location])] = srcLocs map
                (u => (u, dstLocs filter (v =>
                                            conPaths.isPossibleConvoyRoute(u, v)
                                                          && !u.equalsLoosely(v)
                                                          && !u.isAdjacent(v))))
        mwc
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
