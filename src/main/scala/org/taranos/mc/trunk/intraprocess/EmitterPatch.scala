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

import org.taranos.mc.Common.{ReportSectionsParser, Reportable}
import org.taranos.mc.field._
import org.taranos.mc.trunk.intraprocess.EmitterPatch.Channel
import org.taranos.mc.trunk.intraprocess.Signal.{Continuous, SignalTypes}
import org.taranos.mc.trunk.intraprocess.TestableElement.TestableUpdateStateDecoder
import org.taranos.mc.trunk.intraprocess.TrunkElement.{CommonConstructorMetaDecoder, CommonDestructorMetaDecoder,
    CommonQueryDecoder, CommonUpdateMetaDecoder}
import org.taranos.mc.{Cell, CellLogger}
import play.api.libs.json._


object EmitterPatch
{
    import scala.collection.mutable

    class Key (uniqueKey: TrunkElement.UniqueKey, symbol: Symbol = 'EmitterPatch)
        extends Patch.Key(uniqueKey, symbol)

    class Meta (
        uniqueKey: TrunkElement.UniqueKey,
        tag: String,
        badgeOpt: Option[String],
        nameOpt: Option[String],
        descriptionOpt: Option[String])
        extends Patch.Meta[EmitterPatch.Key](
            new EmitterPatch.Key(uniqueKey),
            tag,
            badgeOpt,
            nameOpt,
            descriptionOpt)

    class Attrs (
        var _patchDef: JsObject)
        extends Patch.Attrs
    {
        override
        def Report (sections: ReportSectionsParser): JsObject =
        {
            var report = super.Report(sections)

            report ++=
                Json.obj(FieldModel.Glossary.kEmitterPatchDef -> _patchDef)

            report
        }
    }

    class Refs (
        trunkKey: Trunk.Key,
        tapKey: SignalTap.Key,
        isTapParent: Boolean,
        val _fieldKey: Field.Key,
        val _emitterKey: Emitter.Key,
        val _oscillatorPatchKeys: mutable.HashMap[String, OscillatorPatch.Key] =
            mutable.HashMap.empty[String, OscillatorPatch.Key])
        extends Patch.Refs(
            trunkKey,
            tapKey,
            isTapParent)
    {
        override
        def Report(sections: ReportSectionsParser): JsObject =
        {
            var report = super.Report(sections)

            // Add parent reference:
            report ++=
                Json.obj(FieldModel.Glossary.kEEmitter -> FieldElement.EncodeKey(_emitterKey))

            // Add peer references:
            if (!sections.HasPeerReports)
            {
                var oscillatorPatchKeys = Json.obj()
                _oscillatorPatchKeys.foreach(pair =>
                {
                    val (channelTag, patchKey) = pair
                    oscillatorPatchKeys += channelTag -> JsString(TrunkElement.EncodeKey(patchKey))
                })
                report ++= Json.obj(TrunkModel.Glossary.kEOscillatorPatch -> oscillatorPatchKeys)
            }

            report
        }
    }

    class State (
        val _channels: mutable.HashMap[String, Channel] = mutable.HashMap.empty[String, Channel])
        extends Patch.State
    {
        override
        def Report (sections: ReportSectionsParser): JsObject =
        {
            var report = super.Report(sections)

            var channels = Json.obj()
            _channels.foreach(pair =>
            {
                val (channelTag, channel) = pair
                channels += channelTag -> channel.Report(sections)
            })
            report ++=
                Json.obj(FieldModel.Glossary.kChannel -> channels)

            report
        }
    }

    case class Constructor (
        _tag: String,
        _badgeOpt: Option[String] = None,
        _nameOpt: Option[String] = None,
        _descriptionOpt: Option[String] = None,
        _fieldKey: Field.Key,
        _emitterKey: Emitter.Key,
        _modulatableKey: ModulatableElementKey,
        _tappableKeyOpt: Option[TappableElementKey],
        _patchDef: JsObject)

    case class Destructor (
        _key: EmitterPatch.Key)

    case class Query (
        keys: Vector[EmitterPatch.Key],
        sectionsOpt: Option[String] = None)
        extends TrunkElement.Query[EmitterPatch.Key](
            keys,
            sectionsOpt)

    case class Update (
        _key: EmitterPatch.Key,
        _nameOpt: Option[String] = None,
        _descriptionOpt: Option[String] = None,
        _patchDefOpt: Option[JsObject] = None,
        _signalEncodedOpt: Option[String] = None)

    val kAnyKey = new Key(TrunkModel.Glossary.kAnyKeyBase)

    val kNoneKey = new Key(TrunkModel.Glossary.kNoneKeyBase)

    def DecodeConstructor (encoded: String): Constructor =
    {
        val constructor = Json.parse(encoded)

        val commonMeta = new CommonConstructorMetaDecoder(constructor, Cell.ErrorCodes.EmitterPatchConstructorInvalid)

        val fieldKey: Field.Key =
            (constructor \ TrunkModel.Glossary.kPSRefs \ FieldModel.Glossary.kEField).validate[String] match
            {
                case JsSuccess(value, _) =>
                    val key = FieldElement.DecodeKey[FieldElement.Key](value)
                    key match
                    {
                        case fieldKey: Field.Key => fieldKey

                        case _ =>
                            throw TrunkException(
                                Cell.ErrorCodes.EmitterPatchConstructorInvalid,
                                "invalid emitter key property")
                    }

                case JsError(_) =>
                    throw TrunkException(
                        Cell.ErrorCodes.EmitterPatchConstructorInvalid,
                        "missing emitter key property")
            }

        val emitterKey: Emitter.Key =
            (constructor \ TrunkModel.Glossary.kPSRefs \ FieldModel.Glossary.kEEmitter).validate[String] match
            {
                case JsSuccess(value, _) =>
                    val key = FieldElement.DecodeKey[FieldElement.Key](value)
                    key match
                    {
                        case emitterKey: Emitter.Key => emitterKey

                        case _ =>
                            throw TrunkException(
                                Cell.ErrorCodes.EmitterPatchConstructorInvalid,
                                "invalid emitter key property")
                    }

                case JsError(_) =>
                    throw TrunkException(
                        Cell.ErrorCodes.EmitterPatchConstructorInvalid,
                        "missing emitter key property")
            }

        val modulatableKey: ModulatableElementKey =
            (constructor \ TrunkModel.Glossary.kPSRefs \ TrunkModel.Glossary.kESignalModulator).validate[String] match
            {
                case JsSuccess(value, _) =>
                    val key = TrunkElement.DecodeKey[TrunkElement.Key](value)
                    key match
                    {
                        case modulatableElementKey: ModulatableElementKey => modulatableElementKey

                        case _ =>
                            throw TrunkException(
                                Cell.ErrorCodes.EmitterPatchConstructorInvalid,
                                "invalid modulatable key property")
                    }

                case JsError(_) =>
                    throw TrunkException(
                        Cell.ErrorCodes.EmitterPatchConstructorInvalid,
                        "missing modulatable key property")
            }

        val tappableKeyOpt: Option[TappableElementKey] =
            (constructor \ TrunkModel.Glossary.kPSRefs \ TrunkModel.Glossary.kESignalTap).validate[String] match
            {
                case JsSuccess(value, _) =>
                    val key = TrunkElement.DecodeKey[TrunkElement.Key](value)
                    key match
                    {
                        case tappableElementKey: TappableElementKey => Some(tappableElementKey)

                        case _ =>
                            throw TrunkException(
                                Cell.ErrorCodes.EmitterPatchConstructorInvalid,
                                "invalid tappable key property")
                    }

                case JsError(_) => None
            }

        val patchDef: JsObject =
            (constructor \ TrunkModel.Glossary.kPSAttrs \ FieldModel.Glossary.kEmitterPatchDef).validate[JsObject] match
            {
                case JsSuccess(value, _) => value

                case JsError(_) =>
                    throw TrunkException(
                        Cell.ErrorCodes.OscillatorPatchConstructorInvalid,
                        "invalid emitter patch definition property")
            }

        Constructor(
            commonMeta._tag,
            commonMeta._badgeOpt,
            commonMeta._nameOpt,
            commonMeta._descriptionOpt,
            fieldKey,
            emitterKey,
            modulatableKey,
            tappableKeyOpt,
            patchDef)
    }

    def DecodeDestructor (encoded: String): Destructor =
    {
        val destructor = Json.parse(encoded)

        val commonMeta = new CommonDestructorMetaDecoder[EmitterPatch.Key](
            destructor, Cell.ErrorCodes.EmitterPatchDestructorInvalid)

        Destructor(commonMeta._key)
    }

    def DecodeQuery (encoded: String, isKeysRequired: Boolean = true): Query =
    {
        val query = Json.parse(encoded)

        val commonQuery = new CommonQueryDecoder[EmitterPatch.Key](query, isKeysRequired)

        Query(commonQuery._keysOpt.getOrElse(Vector.empty[EmitterPatch.Key]), commonQuery._sectionsOpt)
    }

    def DecodeUpdate (encoded: String): Update =
    {
        val update = Json.parse(encoded)

        val commonMeta = new CommonUpdateMetaDecoder[EmitterPatch.Key](
            update, Cell.ErrorCodes.EmitterPatchUpdateInvalid)

        val patchDefOpt: Option[JsObject] =
            (update \ TrunkModel.Glossary.kPSAttrs \ FieldModel.Glossary.kEmitterPatchDef).validate[JsObject] match
            {
                case JsSuccess(value, _) => Some(value)

                case JsError(_) => None
            }

        val testableState = new TestableUpdateStateDecoder(update)

        Update(
            commonMeta._key,
            commonMeta._nameOpt,
            commonMeta._descriptionOpt,
            patchDefOpt,
            testableState._signalEncodedOpt)
    }

    class Channel (
        var _channelEnvelope: Envelope.ContinuousEnvelope,
        var _oscillatorPatchDef: JsObject)
        extends Reportable
    {
        override
        def Report (sections: ReportSectionsParser): JsObject =
        {
            var report =
                Json.obj(
                    FieldModel.Glossary.kEnvelope -> _channelEnvelope.Report(sections))

            report ++=
                Json.obj(
                    FieldModel.Glossary.kOscillatorPatchDef -> _oscillatorPatchDef)

            report
        }
    }
}

class EmitterPatch (
    meta: EmitterPatch.Meta,
    attrs: EmitterPatch.Attrs,
    refs: EmitterPatch.Refs)
    (implicit
        protected val _trunkModel: TrunkModel,
        protected val _logger: CellLogger)
    extends Patch[EmitterPatch.Key]
{
    import scala.collection.mutable

    //
    // Meta:
    //
    protected
    val _meta = meta

    def GetMode: Signal.ModeEnum.Mode =
        _meta._mode

    //
    // Attrs:
    //
    protected
    val _attrs = attrs

    //
    // Refs:
    //
    protected
    val _refs = refs

    def BindOscillatorPatch (channelTag: String, key: OscillatorPatch.Key): Unit =
        _refs._oscillatorPatchKeys += channelTag -> key

    def ClearOscillatorPatchKeysMap (): Unit =
        _refs._oscillatorPatchKeys.clear()

    def GetEmitterKey: Emitter.Key =
        _refs._emitterKey

    def GetFieldKey: Field.Key =
        _refs._fieldKey

    def GetOscillatorPatchKeysMap: mutable.HashMap[String, OscillatorPatch.Key] =
        _refs._oscillatorPatchKeys

    def GetTapKey: SignalTap.Key =
        _refs._tapKey

    def IsTapParent: Boolean =
        _refs._isTapParent

    def UnbindOscillatorPatch (channelTag: String): Unit =
        _refs._oscillatorPatchKeys -= channelTag

    //
    // State:
    //
    protected
    val _state = new EmitterPatch.State

    def GetChannelsMap: mutable.HashMap[String, EmitterPatch.Channel] =
        _state._channels

    def ImportChannelDef (channelTag: String, channelDef: JsObject): Unit =
    {
        // Configure a new patch def with the modified channel def and import it:
        val newChannelDefs = (_attrs._patchDef \ FieldModel.Glossary.kChannelDef).validate[JsObject] match
        {
            // Append the modified channel def:
            case JsSuccess (value, _) => value ++ Json.obj(channelTag -> channelDef)

            // If no channel defs exists then something is seriously wrong:
            case JsError(_) =>  throw TrunkException(Cell.ErrorCodes.EmitterPatchInvalid)

        }
        val newPatchDef = _attrs._patchDef ++ Json.obj(FieldModel.Glossary.kChannelDef -> newChannelDefs)
        ImportEmitterPatchDef(Some(newPatchDef))
    }

    def ImportEmitterPatchDef (patchDefOpt: Option[JsObject]):
        Option[mutable.HashMap[String, EmitterPatch.Channel]] =
    {
        val _priorChannelsOpt: Option[mutable.HashMap[String, Channel]] = Some(_state._channels.clone())

        _state._channels.clear()

        val patchDef =
        {
            if (patchDefOpt.isDefined)
                _attrs._patchDef = patchDefOpt.get
            _attrs._patchDef
        }

        val channelDefs: JsObject =
            (patchDef \ FieldModel.Glossary.kChannelDef).validate[JsObject] match
            {
                case JsSuccess(value, _) =>
                    if (value.keys.size < 1)
                        throw TrunkException(Cell.ErrorCodes.EmitterPatchInvalid)
                    value

                case JsError(_) =>
                    throw TrunkException(Cell.ErrorCodes.EmitterPatchInvalid)
            }

        channelDefs.fields.foreach(pair =>
        {
            val (channelTag, channelDef) = pair

            val channelEnvelopeDef: JsObject =
                (channelDef \ FieldModel.Glossary.kEnvelopeDef).validate[JsObject] match
                {
                    case JsSuccess(value, _) => value

                    case JsError(_) => throw TrunkException(Cell.ErrorCodes.EmitterPatchInvalid)
                }
            val channelEnvelope = Envelope.DecodeContinuousEnvelopeDef(channelEnvelopeDef)

            val oscillatorPatchDef: JsObject =
                (channelDef \ FieldModel.Glossary.kOscillatorPatchDef).validate[JsObject] match
                {
                    case JsSuccess(value, _) => value

                    case JsError(_) => throw TrunkException(Cell.ErrorCodes.EmitterPatchInvalid)
                }

            _state._channels += channelTag -> new EmitterPatch.Channel(channelEnvelope, oscillatorPatchDef)
        })

        _priorChannelsOpt
    }

    def MacroCreateChannel (
        channelTag: String,
        envelopeDefOpt: Option[JsObject],
        patchDefOpt: Option[JsObject]): Unit =
    {
        val envelopeDef = envelopeDefOpt.getOrElse(Patch.kDefaultPatchEnvelopeDef)
        val oscillatorPatchDef = patchDefOpt.getOrElse(Patch.kDefaultOscillatorPatchDef)
        val newChannelDef = Json.obj(
            FieldModel.Glossary.kEnvelopeDef -> envelopeDef,
            FieldModel.Glossary.kOscillatorPatchDef -> oscillatorPatchDef)
        val newChannelDefs = (_attrs._patchDef \ FieldModel.Glossary.kChannelDef).as[JsObject] ++
            Json.obj(channelTag -> newChannelDef)
        val newEmitterPatchDef = _attrs._patchDef ++ Json.obj(FieldModel.Glossary.kChannelDef -> newChannelDefs)
        val update = EmitterPatch.Update(_meta._key, _patchDefOpt = Some(newEmitterPatchDef))
        _trunkModel.UpdateEmitterPatches(_refs._trunkKey, Vector(update))
    }

    def MacroDestroyChannel (channelTag: String): Unit =
    {
        val newChannelDefs = (_attrs._patchDef \ FieldModel.Glossary.kChannelDef).as[JsObject] - channelTag
        val newEmitterPatchDef = _attrs._patchDef ++ Json.obj(FieldModel.Glossary.kChannelDef -> newChannelDefs)
        val update = EmitterPatch.Update(_meta._key, _patchDefOpt = Some(newEmitterPatchDef))
        _trunkModel.UpdateEmitterPatches(_refs._trunkKey, Vector(update))
    }

    def MacroSetChannelCeiling (channelTag: String, ceiling: String): Unit =
    {
        val channelDef = (_attrs._patchDef \ FieldModel.Glossary.kChannelDef \ channelTag).validate[JsObject] match
        {
            case JsSuccess (value, _) => value

            case JsError(_) => throw TrunkException(Cell.ErrorCodes.MacroInvalid)
        }
        val newEnvelopeDef = (channelDef \ FieldModel.Glossary.kEnvelopeDef).as[JsObject] ++
            Json.obj(FieldModel.Glossary.kEnvelopeCeiling -> ceiling)
        val newChannelDef = channelDef ++ Json.obj(FieldModel.Glossary.kEnvelopeDef -> newEnvelopeDef)
        ImportChannelDef(channelTag, newChannelDef)
    }

    def MacroSetChannelPoles (channelTag: String, polesPacked: String): Unit =
    {
        val channelDef = (_attrs._patchDef \ FieldModel.Glossary.kChannelDef \ channelTag).validate[JsObject] match
        {
            case JsSuccess (value, _) => value

            case JsError(_) => throw TrunkException(Cell.ErrorCodes.MacroInvalid)
        }
        val newEnvelopeDef = (channelDef \ FieldModel.Glossary.kEnvelopeDef).as[JsObject] ++
            Json.obj(FieldModel.Glossary.kEnvelopePolesPackedDef -> polesPacked) - FieldModel.Glossary.kEnvelopePolesDef
        val newChannelDef = channelDef ++ Json.obj(FieldModel.Glossary.kEnvelopeDef -> newEnvelopeDef)
        ImportChannelDef(channelTag, newChannelDef)
    }

    def MacroSetChannelFloor (channelTag: String, floor: String): Unit =
    {
        val channelDef = (_attrs._patchDef \ FieldModel.Glossary.kChannelDef \ channelTag).validate[JsObject] match
        {
            case JsSuccess (value, _) => value

            case JsError(_) => throw TrunkException(Cell.ErrorCodes.MacroInvalid)
        }
        val newEnvelopeDef = (channelDef \ FieldModel.Glossary.kEnvelopeDef).as[JsObject] ++
            Json.obj(FieldModel.Glossary.kEnvelopeFloor -> floor)
        val newChannelDef = channelDef ++ Json.obj(FieldModel.Glossary.kEnvelopeDef -> newEnvelopeDef)
        ImportChannelDef(channelTag, newChannelDef)
    }

    override
    def Modulate (signal: Signal[_ >: SignalTypes]): ModulatableElement.ModulatedSignals =
    {
        import scala.collection.mutable

        val ordinal = signal._ordinal
        val scalar = signal.asInstanceOf[Signal[Continuous]]._scalar

        val modulatedSignals: mutable.ArrayBuffer[(Option[String], Signal[Continuous])] =
            mutable.ArrayBuffer.empty[(Option[String], Signal[Continuous])]
        _state._channels.foreach(channelPair =>
        {
            val (channelTag, channel) = channelPair
            val envelope = channel._channelEnvelope

            val modulatedScalar = Envelope.ModulateWithContinuousEnvelope(scalar, envelope)
            modulatedSignals += ((Some(channelTag), Signal[Continuous](ordinal, modulatedScalar, Some(GetKey))))
        })
        ModulatableElement.ModulatedSignals(modulatedSignals)
    }

    def Report (sectionsOpt: Option[String] = None): JsObject =
    {
        var report = Json.obj()

        val sections = new ReportSectionsParser(sectionsOpt)

        // Add meta section:
        if (sections.HasMetaPropertyset)
            report ++= Json.obj(TrunkModel.Glossary.kPSMeta -> _meta.Report(sections))

        // Add attrs section:
        if (sections.HasAttrsPropertyset)
            report ++= Json.obj(TrunkModel.Glossary.kPSAttrs -> _attrs.Report(sections))

        // Add refs section:
        if (sections.HasRefsPropertyset)
            report ++= Json.obj(TrunkModel.Glossary.kPSRefs -> _refs.Report(sections))

        // Add state section:
        if (sections.HasStatePropertyset)
            report ++= Json.obj(TrunkModel.Glossary.kPSState -> _state.Report(sections))

        // Add child reports:
        if (_refs._isTapParent && sections.HasChildReports || !_refs._isTapParent && sections.HasPeerReports)
        {
            report ++=
                Json.obj(TrunkModel.Glossary.kRSignalTaps -> _trunkModel.ReportSignalTaps(
                    GetTrunkKey,
                    SignalTap.Query(Vector(_refs._tapKey), sectionsOpt)))
        }

        // Add peer reports:
        if (sections.HasPeerReports)
        {
            var oscillatorReports = Json.obj()
            _refs._oscillatorPatchKeys.foreach(pair =>
            {
                val (channelTag, patchKey) = pair
                val report = _trunkModel.ReportOscillatorPatches(
                    GetTrunkKey,
                    GetKey,
                    OscillatorPatch.Query(Vector(patchKey), sectionsOpt))
                oscillatorReports += (channelTag -> report.head)
            })
            report ++=
                Json.obj(TrunkModel.Glossary.kROscillatorPatches -> oscillatorReports)
        }

        report
    }

    def PropagateTest (signal: Signal[_ >: SignalTypes]): Unit =
    {
        _trunkModel.GetSignalTapOpt(GetTrunkKey, _refs._tapKey) match
        {
            case Some(tap) => tap.Propagate(Some(signal))

            case None => throw TrunkException(Cell.ErrorCodes.EmitterPatchTapless)
        }
    }

    override
    def Activate (): Unit =
    {
        _trunkModel.GetSignalTapOpt(GetTrunkKey, _refs._tapKey) match
        {
            case Some(tap) => tap.Propagate()

            case None => throw TrunkException(Cell.ErrorCodes.EmitterPatchTapless)
        }
    }


//    // Import initial patch definition:
//    _logger.LogDebug(s"EP: importing emitter patch definition")
//    ImportEmitterPatchDef(None)
}

