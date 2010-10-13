/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package GameController

import scala.actors.Actor
import org.json.JSONArray
import org.json.JSONObject
import scala.actors.remote.RemoteActor
import dip.world._
import GameData._
import jdip.messages._
import java.io.File



class GameServer(port : Int, savepath : String) extends Actor {
  

  def act = {
    println("Waiting for messages")
    wait_for_message
  }
  
  private def wait_for_message = react {
    case GameRead(gn, pName) => 
        val ffName : String = savepath + gn
        val f : File = new File(ffName)
        val w : World = World.open(f)
        getGamePhase(w) match {
          case Phase.PhaseType.MOVEMENT => {
              val MovePhase(_, _, cmds) = moveRead(w, pName)
              println(cmds.toString)
          }
          case _ => println("something else!")
        }
    case _ => println("Hello world")
  }

  private def getGamePhase(w : World) : Phase.PhaseType = {
    val tState : TurnState = w.getLastTurnState
    val phase : Phase = tState.getPhase
    phase.getPhaseType
  }

  private def moveRead(w : World, pName : String) : GameData = {
    val tState : TurnState = w.getLastTurnState
    val cPos : Position = tState.getPosition
    val phase : Phase = tState.getPhase
    val m : Map = w.getMap
    val p : Power = m.getClosestPower(pName)
    val mv : MovesWithoutConvoy = new MovesWithoutConvoy(w, pName)
    val mvc : MovesByConvoy = new MovesByConvoy(w, pName)
    val sh : SupportHolds = new SupportHolds(w, pName)
    val sm : SupportMoves = new SupportMoves(w, pName)
    val cn : Convoys = new Convoys(w, pName)
    val provs : List[Province] = cPos.getUnitProvinces(p).toList
    val units : List[Unit] = provs map cPos.getUnit
    val locs : List[Location] = 
      provs zip units map (u => new Location(u._1, u._2.getCoast))

    def constructUnitCommands(u : Location) : JSONObject = {
      val x : JSONObject = new JSONObject
      val moves : JSONArray = new JSONArray
      val supportHolds : JSONArray = new JSONArray
      val convoys : JSONArray = new JSONArray
      val supportMoves : JSONArray = new JSONArray
      mv.getMoves(u):::mvc.getMoves(u) map (v => moves.put(v.toString))
      sh.getSupportHolds(u) map (v => supportHolds.put(v.toString))
      sm.getSupportMoves(u) map (v => {
          val q : JSONObject = new JSONObject
          q.put("src", v._1.toString)
          q.put("dst", v._2.toString)
          supportMoves.put(q)})
      cn.getConvoys(u) map (v => {
          val q : JSONObject = new JSONObject
          q.put("src", v._1.toString)
          q.put("dst", v._2.toString)
          convoys.put(q)
        })
      x.put("unit", u.toString)
      x.put("moves", moves)
      x.put("supportHolds", supportHolds)
      x.put("convoys", convoys)
      x.put("supportMoves", supportMoves)
      x
    }

    val unitCmds : List[JSONObject] = locs map constructUnitCommands
    val cmds : JSONArray = new JSONArray
    unitCmds map cmds.put
    MovePhase(phase.getSeasonType.toString, phase.getYearType.getYear, cmds)
  }

  

}
