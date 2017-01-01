//
// Taranos Cloud Sonification Framework: Service Core
// Copyright (C) 2017 David Hinson, Netrogen Blue LLC (dhinson@netrogenblue.com)
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

package org.taranos.mc.trunk.intraprocess

import org.taranos.mc.Cell
import org.taranos.mc.Common.ReportSectionsParser
import org.taranos.mc.trunk.intraprocess.Signal._
import play.api.libs.json.{JsObject, Json}


object SignalModulator
{
    class Key (uniqueKey: TrunkElement.UniqueKey, symbol: Symbol)
        extends BiasedElement.Key(uniqueKey, symbol)
            with ModulatableElementKey

    abstract
    class Meta[KeyType <: Key] (
        key: KeyType,
        tag: String,
        badgeOpt: Option[String],
        nameOpt: Option[String],
        descriptionOpt: Option[String],
        mode: Signal.ModeEnum.Mode)
        extends BiasedElement.Meta[KeyType](
            key,
            tag,
            badgeOpt,
            nameOpt,
            descriptionOpt,
            mode)

    abstract
    class Attrs
        extends TrunkElement.Attrs

    abstract
    class Refs (
        trunkKey: Trunk.Key,
        var _tapKey: SignalTap.Key,
        val _isTapParent: Boolean)
        extends TrunkElement.Refs(trunkKey)
    {
        override
        def Report (sections: ReportSectionsParser): JsObject =
        {
            var report = super.Report(sections)

            // Add child/peer references:
            if (_isTapParent && !sections.HasChildReports || !_isTapParent && !sections.HasPeerReports)
                report ++=
                    Json.obj(TrunkModel.Glossary.kESignalTap -> TrunkElement.EncodeKey(_tapKey))

            report
        }
    }

    abstract
    class State
        extends TrunkElement.State

    case class ModulatedSignals (
        _signals: Iterable[(Option[String], Signal[_ >: SignalTypes])] =
            Iterable.empty[(Option[String], Signal[_ >: SignalTypes])])

    def GetModulator (
        model: TrunkModel,
        trunkKey: Trunk.Key,
        modulatorKey: SignalModulator.Key): SignalModulator[_ <: SignalModulator.Key] =
    {
        modulatorKey match
        {
            case key: EmitterPatch.Key =>
                model.GetEmitterPatchOpt(trunkKey, key).getOrElse(
                    throw new TrunkException(Cell.ErrorCodes.EmitterPatchInvalid))

            case key: OscillatorPatch.Key =>
                model.GetOscillatorPatchOpt(trunkKey, key).getOrElse(
                    throw new TrunkException(Cell.ErrorCodes.OscillatorPatchInvalid))

            case key: SignalInput.Key =>
                model.GetSignalInputOpt(trunkKey, key).getOrElse(
                    throw new TrunkException(Cell.ErrorCodes.SignalInputInvalid))

            case key: SignalBridge.Key =>
                model.GetSignalBridgeOpt(trunkKey, key).getOrElse(
                    throw new TrunkException(Cell.ErrorCodes.SignalBridgeInvalid))

            case key: SignalOutput.Key =>
                model.GetSignalOutputOpt(trunkKey, key).getOrElse(
                    throw new TrunkException(Cell.ErrorCodes.SignalOutputInvalid))
        }
    }
}

trait SignalModulator[KeyType <: SignalModulator.Key]
    extends BiasedElement[KeyType]
        with ModulatableElement
        with TestableElement
{
    /**
     * Get modulator's routing tap key.
     * @return
     */
    def GetTapKey: SignalTap.Key

    /**
     * Test if modular is parent of a tap.
     */
    def IsTapParent: Boolean

    /**
     * Modulate the given signal into zero or more new signals.
     */
    def Modulate (signal: Signal[_ >: SignalTypes]): SignalModulator.ModulatedSignals =
        SignalModulator.ModulatedSignals()

    /**
     * Trigger a modulation cycle.
     */
    def Trigger (): Unit = {}
}
