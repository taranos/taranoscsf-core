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

package org.taranos.mc.field

import org.taranos.mc.Common.{Real, ReportSectionsParser, Reportable}
import play.api.libs.json.{JsObject, Json}


object Collector
{
    abstract
    class Key (uniqueKey: FieldElement.UniqueKey, symbol: Symbol = 'Collector)
        extends FieldElement.Key(uniqueKey, symbol)

    abstract
    class Meta[KeyType <: Key] (
        key: KeyType,
        tag: String,
        badgeOpt: Option[String],
        nameOpt: Option[String],
        descriptionOpt: Option[String])
        extends FieldElement.Meta[KeyType](
            key,
            tag,
            badgeOpt,
            nameOpt,
            descriptionOpt)

    abstract
    class Attrs (
        // Waveform collector surface area (m^2):
        var _acoustic_a: Real = Collector.Defaults.kAcoustic_a,

        // Lobe bearing envelope poles (packed)
        var _lobeBearingPolesOpt: Option[String] = None,

        // Lobe range (m)
        var _lobeRangeOpt: Option[Real] = None,

        // Lobe range envelope poles (packed)
        var _lobeRangePolesOpt: Option[String] = None,
        
        // Squelch threshold (db)
        var _squelchThresholdOpt: Option[Real] = None)

    extends FieldElement.Attrs
    {
        override
        def Report (sections: ReportSectionsParser): JsObject =
        {
            var report =
                Json.obj(FieldModel.Glossary.kPAttrsAcoustic_a -> Reportable.ReportReal1(_acoustic_a))

            if (_lobeBearingPolesOpt.isDefined)
                report ++= Json.obj(FieldModel.Glossary.kPAttrsLobeBearingPoles -> _lobeBearingPolesOpt.get)

            if (_lobeRangeOpt.isDefined)
                report ++= Json.obj(FieldModel.Glossary.kPAttrsLobeRange -> Reportable.ReportReal3(_lobeRangeOpt.get))

            if (_lobeRangePolesOpt.isDefined)
                report ++= Json.obj(FieldModel.Glossary.kPAttrsLobeRangePoles -> _lobeRangePolesOpt.get)

            if (_squelchThresholdOpt.isDefined)
                report ++= Json.obj(FieldModel.Glossary.kPAttrsSquelchThreshold ->
                    Reportable.ReportReal1(_squelchThresholdOpt.get))

            super.Report(sections) ++ report
        }
    }

    abstract
    class Refs (
        fieldKey: Field.Key,
        val _parentKey: Body.Key)
        extends FieldElement.Refs(fieldKey)
    {
        override
        def Report (sections: ReportSectionsParser): JsObject =
        {
            var report = super.Report(sections)

            // Add parent body ref:
            val parentName = _parentKey._symbol match
            {
                case 'Subject => FieldModel.Glossary.kESubject

                case 'Probe => FieldModel.Glossary.kEProbe

                case _ => ""
            }
            if (parentName.nonEmpty)
                report ++=
                    Json.obj(parentName -> FieldElement.EncodeKey(_parentKey))

            report
        }
    }

    abstract
    class State (
        var _bearingEnvelopeOpt: Option[Envelope.ContinuousEnvelope] = None,
        var _distanceEnvelopeOpt: Option[Envelope.ContinuousEnvelope] = None)
        extends FieldElement.State
    {
        override
        def Report (sections: ReportSectionsParser): JsObject =
        {
            var report = Json.obj()

            if (_bearingEnvelopeOpt.isDefined)
                report ++= Json.obj(FieldModel.Glossary.kPStateBearingEnvelope ->
                    _bearingEnvelopeOpt.get.Report(sections))

            if (_distanceEnvelopeOpt.isDefined)
                report ++= Json.obj(FieldModel.Glossary.kPStateDistanceEnvelope ->
                    _distanceEnvelopeOpt.get.Report(sections))
            
            report
        }
    }

    object Defaults
    {
        // Waveform collector surface area (m^2):
        val kAcoustic_a: Real = 1f

        // Lobe range (m)
        val kLobeRange: Real = 1000f

        // Squelch threshold (db)
        val kSquelchThreshold: Real = 0f
    }
}

trait Collector[KeyType <: Collector.Key]
    extends FieldElement[KeyType]
{
    def GetAcoustic_a: Real

    def GetLobeBearingEnvelopeOpt: Option[Envelope.ContinuousEnvelope]

    def GetLobeRangeEnvelopeOpt: Option[Envelope.ContinuousEnvelope]

    def GetLobeRangeOpt: Option[Real]

    def GetSquelchThresholdOpt: Option[Real]

    def GetParentKey: Body.Key

    def SetAcoustic_a (acoustic_a: Real)

    def SetLobeBearingPolesOpt (lobeBearingPolesOpt: Option[String])

    def SetLobeRangePolesOpt (lobeRangePolesOpt: Option[String])

    def SetLobeRangeOpt (lobeRangeOpt: Option[Real])

    def SetSquelchThresholdOpt (squelchThresholdOpt: Option[Real])
}
