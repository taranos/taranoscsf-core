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

import org.taranos.mc.Cell
import org.taranos.mc.Common.ReportSectionsParser
import org.taranos.mc.trunk.intraprocess.{OscillatorPatch, TrunkElement, TrunkModel}
import play.api.libs.json._


object Oscillator
{
    class Key(uniqueKey: FieldElement.UniqueKey, symbol: Symbol = 'Oscillator)
        extends FieldElement.Key(uniqueKey, symbol)

    abstract
    class Meta[KeyType <: Key](
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
        val _channelTag: String)
        extends FieldElement.Attrs
    {
        override
        def Report (sections: ReportSectionsParser): JsObject =
        {
            var report = super.Report(sections)

            report ++=
                Json.obj(
                    FieldModel.Glossary.kChannel -> _channelTag)

            report
        }
    }

    abstract
    class Refs (
        fieldKey: Field.Key,
        val _parentKey: Emitter.Key,
        var _oscillatorPatchKey: OscillatorPatch.Key)
        extends FieldElement.Refs(fieldKey)
    {
        override
        def Report (sections: ReportSectionsParser): JsObject =
        {
            var report = super.Report(sections)

            val parentName = _parentKey match
            {
                case _: FieldEmitter.Key => FieldModel.Glossary.kEFieldEmitter

                case _: SubjectEmitter.Key => FieldModel.Glossary.kESubjectEmitter

                case _: ProbeEmitter.Key => FieldModel.Glossary.kEProbeEmitter

                case _ => ""
            }
            if (parentName.nonEmpty)
                report ++=
                    Json.obj(parentName -> FieldElement.EncodeKey(_parentKey))

            if (!sections.HasPeerReports)
                report ++=
                    Json.obj(TrunkModel.Glossary.kEOscillatorPatch -> TrunkElement.EncodeKey(_oscillatorPatchKey))

            report
        }
    }

    abstract
    class State
        extends FieldElement.State

    val kNoneKey = new Key(FieldModel.Glossary.kNoneKeyBase)
}

trait Oscillator[KeyType <: Oscillator.Key]
    extends FieldElement[KeyType]
        with CallableElement
{
    def GetParentKey: Emitter.Key

    def GetPatchKey: OscillatorPatch.Key

    override
    def InvokeMacro (makro: JsObject) =
    {
        def CheckArgsCount (
            args: Vector[String],
            minCount: Int,
            maxCount: Int) =
            if (args.size < minCount || args.size > maxCount)
                throw new FieldException(Cell.ErrorCodes.MacroInvalid)

        makro.fields.foreach(field =>
        {
            val (fieldName, fieldValue) = field

            val args: Vector[String] = fieldValue.validate[Vector[String]] match
            {
                case JsSuccess(value, _) => value

                case JsError(errors) => throw new FieldException(Cell.ErrorCodes.MacroInvalid)
            }

            fieldName match
            {
                case "set_loudness_ceiling" =>
                    CheckArgsCount(args, 1, 1)
                    val ceiling = args.head
                    MacroSetLoudnessCeiling(ceiling)

                case "set_loudness_poles" =>
                    CheckArgsCount(args, 1, 1)
                    val polesPacked = args.head
                    MacroSetLoudnessPoles(polesPacked)

                case "set_loudness_floor" =>
                    CheckArgsCount(args, 1, 1)
                    val floor = args.head
                    MacroSetLoudnessFloor(floor)

                case "set_period_poles" =>
                    CheckArgsCount(args, 1, 1)
                    val polesPacked = args.head
                    MacroSetPeriodPoles(polesPacked)

                case "set_pitch_ceiling" =>
                    CheckArgsCount(args, 1, 1)
                    val ceiling = args.head
                    MacroSetPitchCeiling(ceiling)

                case "set_pitch_poles" =>
                    CheckArgsCount(args, 1, 1)
                    val polesPacked = args.head
                    MacroSetPitchPoles(polesPacked)

                case "set_pitch_floor" =>
                    CheckArgsCount(args, 1, 1)
                    val floor = args.head
                    MacroSetPitchFloor(floor)

                case "set_shape_poles" =>
                    CheckArgsCount(args, 1, 1)
                    val polesPacked = args.head
                    MacroSetShapePoles(polesPacked)

                case "set_tone_poles" =>
                    CheckArgsCount(args, 1, 1)
                    val polesPacked = args.head
                    MacroSetTonePoles(polesPacked)

                case "set_waveset_id" =>
                    CheckArgsCount(args, 1, 1)
                    val wavesetId = args.head
                    MacroSetWavesetId(wavesetId)

                case _ => throw new FieldException(Cell.ErrorCodes.MacroInvalid)
            }
        })
    }

    def MacroSetLoudnessCeiling (ceiling: String)

    def MacroSetLoudnessPoles (polesPacked: String)

    def MacroSetLoudnessFloor (floor: String)

    def MacroSetPeriodPoles (polesPacked: String)

    def MacroSetPitchCeiling (ceiling: String)

    def MacroSetPitchPoles (polesPacked: String)

    def MacroSetPitchFloor (floor: String)

    def MacroSetShapePoles (polesPacked: String)

    def MacroSetTonePoles (polesPacked: String)

    def MacroSetWavesetId (wavesetId: String)

    def UnbindPatch()
}
