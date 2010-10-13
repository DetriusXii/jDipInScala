/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package GameData

import dip.world._

class Convoys(w : World, pName : String) {
  val convoys : Q = convoyConstructor

  private type Q = List[(Location, List[(Location, Location)])]
  private def convoyConstructor : Q = {
    def compressList[A](x : List[A], y :List[A]) : List[A] = x:::y
    val tState : TurnState = w.getLastTurnState
    val cPos : Position = tState.getPosition
    val path : Path = new Path(cPos)
    val m : Map = w.getMap
    val p : Power = m.getClosestPower(pName)
    val ps : List[Power] = m.getPowers.toList
    val mwcs : List[MovesByConvoy] = ps map 
                                          (u => new MovesByConvoy(w, u.getName))
    val pairs : List[(Location, Location)]
                                  = (mwcs map (u => u.generateSrcDstPairs))
                    .foldLeft(Nil : List[(Location, Location)])(compressList)
    val fPairs : List[(Location, Location)] =
                pairs filter (u => path.isPossibleConvoyRoute(u._1, u._2))
    val f : Path.FleetFAPEvaluator = new Path.FleetFAPEvaluator(cPos)
    val sp : List[List[Province]] = (fPairs map (u =>
          (Path.findAllSeaPaths(f, u._1.getProvince, u._2.getProvince).toList
           map (u => u.toList.tail.init)).foldLeft(Nil : List[Province])
          (compressList)))
    val provs : List[Province] = cPos.getUnitProvinces(p).toList
    val units : List[Unit] = provs map (cPos.getUnit)
    val uLocs : List[Location] = (provs zip units) map
                                        (u => new Location(u._1, u._2.getCoast))
    val c : Q = uLocs map (u => (u, fPairs zip sp filter
                                 (v => v._2 exists (t => u.isProvinceEqual(t)))
                                                              map (s => s._1)))
    c
  }

  def getConvoys(l : Location) : List[(Location, Location)] =
        (convoys find (u => u._1.equals(l))) match {
          case Some(x) => x._2
          case None => Nil
        }
}
