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

package org.taranos.mc.field

import org.taranos.mc.Common.{Real, ReportSectionsParser, Reportable}
import play.api.libs.json.{JsObject, Json}


object Waveform
{
    class Qualities (
        val _wavesetId: String,
        val _loudnessDecibels: Real,
        val _pitchHertz: Real,
        val _periodExponent: Integer,
        val _shapeNominal: Integer,
        val _toneNominal: Integer)

    class Refs (
        val _bodyKeyOpt: Option[Body.Key],
        val _emitterKey: Emitter.Key,
        val _oscillatorKey: Oscillator.Key)

    class Geometry (
        val _collectorPositionUnit: Body.Position,
        val _emitterPositionUnit: Body.Position,
        val _emitterAbsoluteBearingUnit: Real,
        val _emitterRelativeBearingUnit: Real,
        val _emitterDistanceMeters: Real)
}

class Waveform (
    var _qualities: Waveform.Qualities,
    var _refs: Waveform.Refs,
    var _geometryOpt: Option[Waveform.Geometry])
    extends Reportable
{
    //
    // Reports:
    //
    override
    def Report (sections: ReportSectionsParser): JsObject =
    {
        // Add qualities (q) section:
        val qualities: JsObject = Json.obj(
            FieldModel.Glossary.kQWavesetId -> _qualities._wavesetId,
            FieldModel.Glossary.kQLoudness -> Reportable.ReportReal1(_qualities._loudnessDecibels),
            FieldModel.Glossary.kQPitch -> Reportable.ReportReal1(_qualities._pitchHertz),
            FieldModel.Glossary.kQPeriod -> Reportable.ReportInteger(_qualities._periodExponent),
            FieldModel.Glossary.kQShape -> Reportable.ReportInteger(_qualities._shapeNominal),
            FieldModel.Glossary.kQTone -> Reportable.ReportInteger(_qualities._toneNominal))
        var report: JsObject =
            Json.obj(FieldModel.Glossary.kRQualities -> qualities)

        // Add refs (r) section:
        var refs = Json.obj(
            FieldModel.Glossary.kEEmitter -> FieldElement.EncodeKey(_refs._emitterKey),
            FieldModel.Glossary.kEOscillator -> FieldElement.EncodeKey(_refs._oscillatorKey))
        if (_refs._bodyKeyOpt.isDefined)
            refs = refs ++ Json.obj(FieldModel.Glossary.kEBody -> FieldElement.EncodeKey(_refs._bodyKeyOpt.get))
        if (sections.HasRefsPropertyset)
            report ++= Json.obj(FieldModel.Glossary.kPSRefs -> refs)

        // Add geo (g) section:
        if (sections.HasGeoPropertyset && _geometryOpt.isDefined)
        {
            val geometry = _geometryOpt.get
            report ++=
                Json.obj(FieldModel.Glossary.kPSGeo -> Json.obj(
                    FieldModel.Glossary.kPGeoCollectorPosition -> Json.arr(
                        Reportable.ReportReal3(geometry._collectorPositionUnit._x),
                        Reportable.ReportReal3(geometry._collectorPositionUnit._y)),
                    FieldModel.Glossary.kPGeoEmitterPosition -> Json.arr(
                        Reportable.ReportReal3(geometry._emitterPositionUnit._x),
                        Reportable.ReportReal3(geometry._emitterPositionUnit._y)),
                    FieldModel.Glossary.kPGeoEmitterBearingAbsolute -> Reportable.ReportReal3(geometry._emitterAbsoluteBearingUnit),
                    FieldModel.Glossary.kPGeoEmitterBearingRelative -> Reportable.ReportReal3(geometry._emitterRelativeBearingUnit),
                    FieldModel.Glossary.kPGeoEmitterDistance -> Reportable.ReportReal1(geometry._emitterDistanceMeters)))
        }

        report
    }
}
