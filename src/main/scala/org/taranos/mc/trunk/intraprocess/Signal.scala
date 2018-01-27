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

package org.taranos.mc.trunk.intraprocess

import org.taranos.mc.Cell
import org.taranos.mc.Common.{Propertyset, ReportSectionsParser, Reportable}
import org.taranos.mc.trunk.intraprocess.Signal.{Continuous, Discrete}
import play.api.libs.json.{JsObject, JsString, Json}


object Signal
{
    type Continuous = Double

    type Discrete = Int

    type Virtual = Unit

    type SignalTypes = Continuous with Discrete with Virtual

    object ModeEnum
        extends Enumeration
    {
        type Mode = Value

        val
            /**
             * Bias unknown (error).
             */
            Null,

            /**
             * Accepts only continuous type signals.
             */
            Continuous,

            /**
             * Accepts only discrete type signals.
             */
            Discrete,

            /**
             * Accepts any type of signals.
             */
            Unbiased

        = Value

        /**
         * Convert a mode friendly name to a mode enum.
         * @param friendly Mode friendly name.
         */
        def FromFriendly (friendly: String): Mode =
            friendly match
            {
                case TrunkModel.Glossary.kSignalModeContinuous => Continuous
                case TrunkModel.Glossary.kSignalModeDiscrete => Discrete
                case TrunkModel.Glossary.kSignalModeUnbiased => Unbiased
                case _ => Null
            }

        /**
         * Convert a mode enum to a mode friendly name.
         * @param mode Mode enum.
         */
        def ToFriendly (mode: Mode): String =
            mode match
            {
                case Continuous => TrunkModel.Glossary.kSignalModeContinuous
                case Discrete => TrunkModel.Glossary.kSignalModeDiscrete
                case Unbiased => TrunkModel.Glossary.kSignalModeUnbiased
                case Null => assert(false); null
            }
    }

    def DecodeSignal (
        trunk: Trunk,
        encoded: String,
        decoderMode: ModeEnum.Mode): Signal[_ >: SignalTypes] =
    {
        val trunkModel = trunk.GetTrunkModel

        val parts = encoded.split(TrunkModel.Glossary.kPartSeparator)
        val signal: Signal[_ >: SignalTypes] = parts.length match
        {
            // Single-part format = "<scalar>"
            case 1 =>
                decoderMode match
                {
                    case ModeEnum.Continuous =>
                        val scalar = parts(0).toDouble
                        trunkModel.CreateSignal[Continuous](scalar)

                    case ModeEnum.Discrete =>
                        val scalar = parts(0).toInt
                        trunkModel.CreateSignal[Discrete](scalar)

                    case _ =>
                        throw new TrunkException(Cell.ErrorCodes.SignalModeIndeterminate)
                }

            // Two-part format = "<scalar>~<scalar type>"
            case 2 =>
                val signalMode = ModeEnum.FromFriendly(parts(1))
                signalMode match
                {
                    case ModeEnum.Continuous =>
                        val scalar = parts(0).toDouble
                        trunkModel.CreateSignal[Continuous](scalar)

                    case ModeEnum.Discrete =>
                        val scalar = parts(0).toInt
                        trunkModel.CreateSignal[Discrete](scalar)

                    case _ =>
                        throw new TrunkException(Cell.ErrorCodes.SignalModeIndeterminate)
                }

            case _ =>
                throw new TrunkException(Cell.ErrorCodes.SignalInvalid)
        }

        // Return signal:
        signal
    }
}

/**
 * Signal (s)
 */

case class Signal[Mode >: Signal.SignalTypes] (
    _ordinal: Int,
    _scalar: Mode,
    var _propagatorKeyOpt: Option[TrunkElement.Key] = None)
    extends Propertyset
{
    override
    def Report (sections: ReportSectionsParser): JsObject =
    {
        val report = Json.obj(
            TrunkModel.Glossary.kSignalOrdinal -> Reportable.ReportInteger(_ordinal),

            TrunkModel.Glossary.kSignalMode -> JsString(
                _scalar match
                {
                    case _: Continuous => TrunkModel.Glossary.kSignalModeContinuous

                    case _: Discrete => TrunkModel.Glossary.kSignalModeDiscrete

                    case _ => "?"
                }),

            TrunkModel.Glossary.kSignalValue -> JsString(
                _scalar match
                {
                    case _: Continuous => Reportable.ReportReal3(_scalar.asInstanceOf[Continuous])

                    case _: Discrete => Reportable.ReportInteger(_scalar.asInstanceOf[Discrete])

                    case _ => "?"
                }))

            report
    }

    def ToFriendly: String = s"{${_ordinal},${_scalar}"
}
