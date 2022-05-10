package lab.demo

import it.unibo.scafi.incarnations.BasicAbstractIncarnation
import it.unibo.scafi.simulation.s2.frontend.incarnation.scafi.bridge.ExportEvaluation.EXPORT_EVALUATION
import it.unibo.scafi.simulation.s2.frontend.incarnation.scafi.bridge.SimulationInfo
import it.unibo.scafi.simulation.s2.frontend.incarnation.scafi.configuration.{ScafiProgramBuilder, ScafiWorldInformation}
import it.unibo.scafi.simulation.s2.frontend.incarnation.scafi.world.ScafiWorldInitializer.Random
import it.unibo.scafi.simulation.s2.frontend.incarnation.scafi.bridge.ScafiWorldIncarnation.EXPORT
import it.unibo.scafi.simulation.s2.frontend.view.{ViewSetting, WindowConfiguration}
import lab.gui.patch.RadiusLikeSimulation
import it.unibo.scafi.space.graphics2D.BasicShape2D.Circle

object Incarnation extends BasicAbstractIncarnation
import lab.demo.Incarnation._ //import all stuff from an incarnation
import lab.demo.Incarnation.Builtins.Bounded;

object Simulation extends App {

  val formatter_evaluation: EXPORT_EVALUATION[Any] = (e : EXPORT) => formatter(e.root[Any]())

  val formatter: Any => Any = (e) => e match {
    case (a,b) => (formatter(a),formatter(b))
    case (a,b,c) => (formatter(a),formatter(b),formatter(c))
    case (a,b,c,d) => (formatter(a),formatter(b),formatter(c),formatter(d))
    case l:Iterable[_] => l.map(formatter(_)).toString
    case i: java.lang.Number if (i.doubleValue()>100000) => "Inf"
    case i: java.lang.Number if (-i.doubleValue()>100000) => "-Inf"
    case i: java.lang.Double => f"${i.doubleValue()}%1.2f"
    case x => x.toString
  }

  val programClass = classOf[Main]
  val nodes = 500
  val neighbourRange = 200
  val (width, height) = (1920, 1080)
  ViewSetting.windowConfiguration = WindowConfiguration(width, height)
  ScafiProgramBuilder (
    Random(nodes, width, height),
    SimulationInfo(programClass,exportEvaluations = List(formatter_evaluation)),
    RadiusLikeSimulation(neighbourRange),
    ScafiWorldInformation(shape = Some(Circle(5,5))),
    neighbourRender = true,
  ).launch()
}

class Main extends AggregateProgram with StandardSensors {

  case class Msg(distance: Double, id: Int, symBreaker: Int)
  implicit object BoundedMsg extends Bounded[Msg]{
    override def bottom: Msg = Msg(0.0, mid, mid)
    override def top: Msg = Msg(0.0, mid, Int.MaxValue)
    override def compare(a: Msg, b: Msg): Int =
      if (a.symBreaker == b.symBreaker)
        a.distance.compareTo(b.distance)
      else
        a.symBreaker.compareTo(b.symBreaker)
  }

  def S(grain: Double): ID =
    rep(Msg(0.0, mid, mid)){ case Msg(d,i,s) =>
      minHood[Msg]{
        Msg(nbr{d} + nbrRange, nbr{i}, nbr{s}) match {
          case Msg(_, id, _) if (id == mid) => implicitly[Bounded[Msg]].bottom
          case Msg(dd, _, _) if (dd >= grain) => implicitly[Bounded[Msg]].top
          case m => m
        }
      }
    }.id

  override def main() = S(300)
}