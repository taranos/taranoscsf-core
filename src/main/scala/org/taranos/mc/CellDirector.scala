//
// Taranos Cloud Sonification Framework: Service Core
// Copyright (C) 2018 David Hinson, Netrogen Blue LLC (dhinson@netrogenblue.com)
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

package org.taranos.mc

import org.taranos.common._


object CellDirector
{
    val kActorName = "CellDirector"

    trait CellDirectorMessage
        extends Message

    object RequestMessages
    {
        case object Start
            extends SupervisionMessage

        case object Stop
            extends SupervisionMessage

        case class ServiceCall (_serviceCall: org.taranos.common.ServiceCall)
            extends CellDirectorMessage
    }

    object ResponseMessages
    {
        case object Started
            extends SupervisionMessage
    }

    def MakeProps: akka.actor.Props =
        akka.actor.Props[CellDirector]
}

class CellDirector
    extends akka.actor.Actor
        with Director
{
    import scala.collection.mutable

    private
    var _defaultCellKey: Option[Cell.Key] = None

    private
    var _cells = mutable.HashMap.empty[Cell.Key, akka.actor.ActorRef]

    private
    val _log = akka.event.Logging(context.system, this)

    private
    def CreateCell (tag: String): Cell.Key =
    {
        // Create cell actor:
        val uniqueKey = Cell.MakeUniqueKey(tag)
        val cell = context.actorOf(Cell.MakeActorProps(uniqueKey), Cell.MakeActorName(uniqueKey))

        // Add cell to cells map:
        val cellKey = new Cell.Key(uniqueKey)
        if (_cells.isEmpty)
            _defaultCellKey = Some(cellKey)
        _cells += cellKey -> cell

        // Watch cell:
        context.watch(cell)

        // Tell cell to start:
        cell ! Cell.RequestMessages.Start

        cellKey
    }

    private
    def HandleMessage_Start (): Unit =
    {
        // Create default cell:
        CreateCell(Cell.Glossary.kEDefaultCell)

        // Tell supervisor we've started:
        sender ! CellDirector.ResponseMessages.Started
    }

    private
    def HandleMessage_ServiceCall (serviceCall: ServiceCall): Unit =
    {
        // TBD:  Lookup cell key per access principal. For now we assume an implicit principal with an implicit cell
        // so we'll fail over to default cell handling:
        var cellKeyOpt: Option[Cell.Key] = None

        // If no cell key provided then assume default cell key:
        if (cellKeyOpt.isEmpty)
            cellKeyOpt = _defaultCellKey

        // Lookup cell ref per cell key:
        val cellRef: akka.actor.ActorRef = cellKeyOpt match
        {
            case Some(cellKey) =>
                val cellOpt = _cells.get(cellKey)
                cellOpt.getOrElse(throw CellException(Cell.ErrorCodes.CellUnknown))

            case None => throw CellException(Cell.ErrorCodes.CellKeyInvalid)
        }

        // Forward request to cell:
        cellRef.tell(Cell.RequestMessages.ServiceCall(serviceCall), sender())
    }

    //
    // akka.actor.Actor
    //

    override
    def postRestart (reason: Throwable): Unit =
    {
        super.postRestart(reason)
    }

    override
    def postStop (): Unit =
    {
        super.postStop()
    }

    override
    def preRestart (
        reason: Throwable,
        message: Option[Any]): Unit =
    {
        super.preRestart(reason, message)
    }

    override
    def preStart (): Unit =
    {
        super.preStart()
    }

    def receive: Receive =
    {
        //
        // From supervisor:
        //

        case CellDirector.RequestMessages.Start =>
            HandleMessage_Start()

        //
        // From supervised:
        //

        case Cell.ResponseMessages.Started =>
            _log.info(s"Cell started (${sender()}")

        //
        // From clients:
        //

        case CellDirector.RequestMessages.ServiceCall (serviceCall) =>
            HandleMessage_ServiceCall(serviceCall)

        //
        // From self:
        //

        //
        // Other:
        //

        case akka.actor.Terminated(who) =>
            _log.warning(s"child death ($who)")

        case unhandled =>
            _log.warning(s"received unhandled message (${unhandled.getClass})")
            assert(false)
    }
}
