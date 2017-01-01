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

import org.taranos.mc.Cell
import org.taranos.mc.Common.ReportSectionsParser
import org.taranos.mc.trunk.intraprocess.{EmitterPatch, TrunkElement, TrunkModel}
import play.api.libs.json.{JsError, JsObject, JsSuccess, Json}


object Emitter
{
    import scala.collection.mutable

    class Key (uniqueKey: FieldElement.UniqueKey, symbol: Symbol = 'Emitter)
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
    class Attrs
        extends FieldElement.Attrs

    abstract
    class Refs[ParentKeyType <: FieldElement.Key, ChildKeyType <: Oscillator.Key] (
        fieldKey: Field.Key,
        val _parentKey: FieldElement.Key,
        var _emitterPatchKey: EmitterPatch.Key,
        val _oscillatorKeys: mutable.Set[ChildKeyType] = mutable.Set.empty[ChildKeyType],
        var _defaultOscillatorKey: Oscillator.Key = Oscillator.kNoneKey)
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

                case _ => ""    // Case for field emitters
            }
            if (parentName.nonEmpty)
                report ++=
                    Json.obj(parentName -> FieldElement.EncodeKey(_parentKey))

            // Add default oscillator ref:
            val oscillatorName = _defaultOscillatorKey._symbol match
            {
                case 'FieldOscillator => FieldModel.Glossary.kEDefaultFieldOscillator

                case 'SubjectOscillator => FieldModel.Glossary.kEDefaultSubjectOscillator

                case 'ProbeOscillator => FieldModel.Glossary.kEDefaultProbeOscillator

                case _ => ""
            }
            if (oscillatorName.nonEmpty)
                report ++=
                    Json.obj(oscillatorName -> FieldElement.EncodeKey(_defaultOscillatorKey))

            // Add child refs:
            if (!sections.HasChildReports)
            {
                // Add oscillator refs:
                val childName = _defaultOscillatorKey._symbol match
                {
                    case 'FieldOscillator => FieldModel.Glossary.kEFieldOscillator

                    case 'SubjectOscillator => FieldModel.Glossary.kESubjectOscillator

                    case 'ProbeOscillator => FieldModel.Glossary.kEProbeOscillator

                    case _ => ""
                }
                if (childName.nonEmpty)
                    report ++=
                        Json.obj(childName -> _oscillatorKeys.map(FieldElement.EncodeKey))
            }

            // Add emitter patch ref:
            if (!sections.HasPeerReports)
                report ++=
                    Json.obj(TrunkModel.Glossary.kEEmitterPatch -> TrunkElement.EncodeKey(_emitterPatchKey))

            report
        }
    }

    abstract
    class State
        extends FieldElement.State

    val kNoneKey = new Key(FieldModel.Glossary.kNoneKeyBase)
}

trait Emitter[KeyType <: Emitter.Key]
    extends FieldElement[KeyType]
        with CallableElement
{
    def BindOscillator (oscillatorKey: Oscillator.Key)

    def BindPatch (emitterPatchKey: EmitterPatch.Key)

    def Emit (): Unit

    def GetDefaultOscillatorKey: Oscillator.Key

    def GetOscillatorKeys: Set[_ <: Oscillator.Key]

    def GetParentKey: FieldElement.Key

    def GetPatchKey: EmitterPatch.Key

    override
    def InvokeMacro (makro: JsObject) =
    {
        def CheckArgsCount (
            args: Vector[String],
            minCount: Int,
            maxCount: Int) =
            if (args.length < minCount || args.length > maxCount)
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
                case "create_channel" =>
                    CheckArgsCount(args, 1, 3)
                    val channelTag = args.head
                    val envelopeDefOpt =
                        if (args.length > 1)
                            Json.parse(args(1)).validate[JsObject] match
                            {
                                case JsSuccess(value, _) => Some(value)

                                case JsError(errors) => throw new FieldException(Cell.ErrorCodes.MacroInvalid)
                            }
                        else
                            None
                    val patchDefOpt =
                        if (args.length > 2)
                            Json.parse(args(2)).validate[JsObject] match
                            {
                                case JsSuccess(value, _) => Some(value)

                                case JsError(errors) => throw new FieldException(Cell.ErrorCodes.MacroInvalid)
                            }
                        else
                            None
                    MacroCreateChannel(
                        channelTag, envelopeDefOpt, patchDefOpt)

                case "destroy_channel" =>
                    CheckArgsCount(args, 1, 1)
                    val channelTag = args.head
                    MacroDestroyChannel(channelTag)

                case "set_channel_ceiling" =>
                    CheckArgsCount(args, 2, 2)
                    val channelTag = args.head
                    val ceiling = args(1)
                    MacroSetChannelCeiling(channelTag, ceiling)

                case "set_channel_poles" =>
                    CheckArgsCount(args, 2, 2)
                    val channelTag = args.head
                    val polesPacked = args(1)
                    MacroSetChannelPoles(channelTag, polesPacked)

                case "set_channel_floor" =>
                    CheckArgsCount(args, 2, 2)
                    val channelTag = args.head
                    val floor = args(1)
                    MacroSetChannelFloor(channelTag, floor)

                case _ => throw new FieldException(Cell.ErrorCodes.MacroInvalid)
            }
        })
    }

    def MacroCreateChannel (
        channelTag: String,
        envelopeDefOpt: Option[JsObject],
        patchDefOpt: Option[JsObject])

    def MacroDestroyChannel (channelTag: String)

    def MacroSetChannelCeiling (channelTag: String, ceiling: String)

    def MacroSetChannelPoles (channelTag: String, polesPacked: String)

    def MacroSetChannelFloor (channelTag: String, floor: String)

    def UnbindOscillator (
        oscillatorKey: Oscillator.Key,
        isForcedUnbind: Boolean): Boolean

    def UnbindPatch ()
}
