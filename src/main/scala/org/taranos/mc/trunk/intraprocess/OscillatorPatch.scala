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

import org.taranos.mc.Common.{ReportSectionsParser, Reporter}
import org.taranos.mc.field._
import org.taranos.mc.trunk.intraprocess.Signal.{Continuous, Discrete, SignalTypes}
import org.taranos.mc.trunk.intraprocess.TestableElement.TestableUpdateStateDecoder
import org.taranos.mc.trunk.intraprocess.TrunkElement.{CommonConstructorMetaDecoder, CommonDestructorMetaDecoder, CommonQueryDecoder, CommonUpdateMetaDecoder}
import org.taranos.mc.{Cell, CellLogger}
import play.api.libs.json.{JsError, JsObject, JsSuccess, Json}


object OscillatorPatch
{
    class Key (uniqueKey: TrunkElement.UniqueKey, symbol: Symbol = 'OscillatorPatch)
        extends Patch.Key(uniqueKey, symbol)

    class Meta (
        uniqueKey: TrunkElement.UniqueKey,
        tag: String,
        badgeOpt: Option[String],
        nameOpt: Option[String],
        descriptionOpt: Option[String])
        extends Patch.Meta[OscillatorPatch.Key](
            new OscillatorPatch.Key(uniqueKey),
            tag,
            badgeOpt,
            nameOpt,
            descriptionOpt)

    class Attrs (
        val _channelTag: String,
        var _patchDef: JsObject)
        extends Patch.Attrs
    {
        override
        def Report (sections: ReportSectionsParser): JsObject =
        {
            var report = super.Report(sections)

            report ++=
                Json.obj(
                    FieldModel.Glossary.kChannelTag -> _channelTag)

            report ++=
                Json.obj(
                    FieldModel.Glossary.kOscillatorPatchDef -> _patchDef)

            report
        }
    }

    class Refs (
        trunkKey: Trunk.Key,
        tapKey: SignalTap.Key,
        isTapParent: Boolean,
        val _fieldKey: Field.Key,
        var _oscillatorKey: Oscillator.Key = Oscillator.kNoneKey,
        var _emitterPatchKey: EmitterPatch.Key = EmitterPatch.kNoneKey,
        var _loudnessOutputKey: SignalOutput.Key = SignalOutput.kNoneKey,
        var _periodOutputKey: SignalOutput.Key = SignalOutput.kNoneKey,
        var _pitchOutputKey: SignalOutput.Key = SignalOutput.kNoneKey,
        var _shapeOutputKey: SignalOutput.Key = SignalOutput.kNoneKey,
        var _toneOutputKey: SignalOutput.Key = SignalOutput.kNoneKey)
        extends Patch.Refs(
            trunkKey,
            tapKey,
            isTapParent)
    {
        override
        def Report (sections: ReportSectionsParser): JsObject =
        {
            var report = super.Report(sections)

            // Add parent reference:
            report ++=
                Json.obj(FieldModel.Glossary.kEOscillator -> FieldElement.EncodeKey(_oscillatorKey))

            // Add child references:
            if (!sections.HasChildReports)
                report ++=
                    Json.obj(TrunkModel.Glossary.kESignalOutput ->
                        Json.obj(
                            FieldModel.Glossary.kQLoudness -> TrunkElement.EncodeKey(_loudnessOutputKey),
                            FieldModel.Glossary.kQPitch -> TrunkElement.EncodeKey(_pitchOutputKey),
                            FieldModel.Glossary.kQPeriod -> TrunkElement.EncodeKey(_periodOutputKey),
                            FieldModel.Glossary.kQShape -> TrunkElement.EncodeKey(_shapeOutputKey),
                            FieldModel.Glossary.kQTone -> TrunkElement.EncodeKey(_toneOutputKey)))

            report
        }
    }

    class State (
        var _wavesetId: String = "",
        var _loudnessEnvelope: Envelope.ContinuousEnvelope = new Envelope.ContinuousEnvelope,
        var _pitchEnvelope: Envelope.ContinuousEnvelope = new Envelope.ContinuousEnvelope,
        var _periodEnvelope: Envelope.DiscreteEnvelope = new Envelope.DiscreteEnvelope,
        var _shapeEnvelope: Envelope.DiscreteEnvelope = new Envelope.DiscreteEnvelope,
        var _toneEnvelope: Envelope.DiscreteEnvelope = new Envelope.DiscreteEnvelope)
        extends Patch.State
    {
        override
        def Report (sections: ReportSectionsParser): JsObject =
        {
            var report = super.Report(sections)

            report ++=
                Json.obj(FieldModel.Glossary.kQWavesetId -> _wavesetId)

            report ++=
                Json.obj(FieldModel.Glossary.kEnvelope -> Json.obj(
                    FieldModel.Glossary.kQLoudness -> _loudnessEnvelope.Report(sections),
                    FieldModel.Glossary.kQPitch -> _pitchEnvelope.Report(sections),
                    FieldModel.Glossary.kQPeriod -> _periodEnvelope.Report(sections),
                    FieldModel.Glossary.kQShape -> _shapeEnvelope.Report(sections),
                    FieldModel.Glossary.kQTone -> _toneEnvelope.Report(sections)))

            report
        }
    }

    case class Constructor (
        _tag: String,
        _badgeOpt: Option[String] = None,
        _nameOpt: Option[String] = None,
        _descriptionOpt: Option[String] = None,
        _fieldKey: Field.Key,
        _emitterPatchKey: EmitterPatch.Key,
        _channelTag: String,
        _tappableKeyOpt: Option[TappableElementKey],
        _patchDef: JsObject)

    case class Destructor (
        _key: OscillatorPatch.Key)

    case class Query (
        keys: Vector[OscillatorPatch.Key],
        sectionsOpt: Option[String] = None)
        extends TrunkElement.Query[OscillatorPatch.Key](
            keys,
            sectionsOpt)

    case class Update (
        _key: OscillatorPatch.Key,
        _nameOpt: Option[String],
        _descriptionOpt: Option[String],
        _patchDefOpt: Option[JsObject] = None,
        _signalEncodedOpt: Option[String])

    class EnvelopeReporter (
        patchDef: JsObject,
        oscillatorEnvelopeKey: OscillatorEnvelope.Key)
        extends Reporter
    {
        def Report (sectionsOpt: Option[String] = None): JsObject =
        {
            var report = Json.obj()

//            val sections = new ReportSectionsParser(sectionsOpt)

            report ++=
                ((patchDef \ FieldModel.Glossary.kEnvelopeDef \
                    oscillatorEnvelopeKey._uniqueKey).validate[JsObject] match
                {
                    case JsSuccess(value, _) => value

                    case JsError(errors) => Json.obj()  // Not sure what to do with this...
                })

            report
        }
    }

    val kAnyKey = new Key(TrunkModel.Glossary.kAnyKeyBase)

    val kNoneKey = new Key(TrunkModel.Glossary.kNoneKeyBase)

    def DecodeConstructor (encoded: String): Constructor =
    {
        val constructor = Json.parse(encoded)

        val commonMeta = new CommonConstructorMetaDecoder(
            constructor, Cell.ErrorCodes.OscillatorPatchConstructorInvalid)

        val channelTag: String =
            (constructor \ TrunkModel.Glossary.kPSAttrs \ FieldModel.Glossary.kChannelTag).validate[String] match
            {
                case JsSuccess(value, _) => value

                case JsError(errors) =>
                    throw new TrunkException(
                        Cell.ErrorCodes.OscillatorPatchConstructorInvalid,
                        "missing emitter channel tag property")
            }

        val fieldKey: Field.Key =
            (constructor \ TrunkModel.Glossary.kPSRefs \ FieldModel.Glossary.kEField).validate[String] match
            {
                case JsSuccess(value, _) =>
                    val key = FieldElement.DecodeKey[FieldElement.Key](value)
                    key match
                    {
                        case fieldKey: Field.Key => fieldKey

                        case _ =>
                            throw new TrunkException(
                                Cell.ErrorCodes.EmitterPatchConstructorInvalid,
                                "invalid emitter key property")
                    }

                case JsError(errors) =>
                    throw new TrunkException(
                        Cell.ErrorCodes.EmitterPatchConstructorInvalid,
                        "missing emitter key property")
            }

        val emitterPatchKey: EmitterPatch.Key =
            (constructor \ TrunkModel.Glossary.kPSRefs \ TrunkModel.Glossary.kEEmitterPatch).validate[String] match
            {
                case JsSuccess(value, _) =>
                    TrunkElement.DecodeKey[EmitterPatch.Key](value)

                case JsError(errors) =>
                    throw new TrunkException(
                        Cell.ErrorCodes.OscillatorPatchConstructorInvalid,
                        "missing modulatable key property")
            }

        val tappableKeyOpt: Option[TappableElementKey] =
            (constructor \ TrunkModel.Glossary.kPSRefs \ TrunkModel.Glossary.kESignalTap).validate[String] match
            {
                case JsSuccess(value, _) =>
                    val key = TrunkElement.DecodeKey[TrunkElement.Key](value)
                    key match

                    {
                        case _: TappableElementKey => Some(key.asInstanceOf[TappableElementKey])

                        case _ =>
                            throw new TrunkException(
                                Cell.ErrorCodes.OscillatorPatchConstructorInvalid,
                                "invalid tappable key property")
                    }

                case JsError(errors) => None
            }

        val patchDef: JsObject =
            (constructor \ TrunkModel.Glossary.kPSAttrs \
                FieldModel.Glossary.kOscillatorPatchDef).validate[JsObject] match
            {
                case JsSuccess(value, _) => value

                case JsError(errors) =>
                    throw new TrunkException(
                        Cell.ErrorCodes.OscillatorPatchConstructorInvalid,
                        "invalid oscillator patch definition property")
            }

        new Constructor(
            commonMeta._tag,
            commonMeta._badgeOpt,
            commonMeta._nameOpt,
            commonMeta._descriptionOpt,
            fieldKey,
            emitterPatchKey,
            channelTag,
            tappableKeyOpt,
            patchDef)
    }

    def DecodeDestructor (encoded: String): Destructor =
    {
        val destructor = Json.parse(encoded)

        val commonMeta = new CommonDestructorMetaDecoder[OscillatorPatch.Key](
            destructor, Cell.ErrorCodes.OscillatorPatchDestructorInvalid)

        new Destructor(commonMeta._key)
    }

    def DecodeQuery (encoded: String): Query =
    {
        val query = Json.parse(encoded)

        val commonQuery = new CommonQueryDecoder[OscillatorPatch.Key](query)

        new Query(commonQuery._keysOpt.get, commonQuery._sectionsOpt)
    }

    def DecodeUpdate (encoded: String): Update =
    {
        val update = Json.parse(encoded)

        val commonMeta = new CommonUpdateMetaDecoder[OscillatorPatch.Key](
            update, Cell.ErrorCodes.OscillatorPatchUpdateInvalid)
        
        val patchDefOpt: Option[JsObject] =
            (update \ TrunkModel.Glossary.kPSAttrs \ FieldModel.Glossary.kOscillatorPatchDef).validate[JsObject] match
            {
                case JsSuccess(value, _) => Some(value)

                case JsError(errors) => None
            }

        val testableState = new TestableUpdateStateDecoder(update)

        new Update(
            commonMeta._key,
            commonMeta._nameOpt,
            commonMeta._descriptionOpt,
            patchDefOpt,
            testableState._signalEncodedOpt)
    }
}

class OscillatorPatch (
    meta: OscillatorPatch.Meta,
    attrs: OscillatorPatch.Attrs,
    refs: OscillatorPatch.Refs)
    (implicit
        protected val _trunkModel: TrunkModel,
        protected val _logger: CellLogger)
    extends SignalModulator[OscillatorPatch.Key]
{
    //
    // Meta:
    //
    protected
    val _meta = meta

    def GetMode = _meta._mode

    //
    // Attrs:
    //
    protected
    val _attrs = attrs

    def GetChannelTag = _attrs._channelTag

    //
    // Refs:
    //
    protected
    val _refs = refs

    def GetEmitterPatchKey: EmitterPatch.Key = _refs._emitterPatchKey

    def GetFieldKey: Field.Key = _refs._fieldKey

    def GetTapKey: SignalTap.Key = _refs._tapKey

    def BindLoudnessOutput(outputKey: SignalOutput.Key) = _refs._loudnessOutputKey = outputKey

    def GetLoudnessOutputKey = _refs._loudnessOutputKey

    def UnbindLoudnessOutput() = _refs._loudnessOutputKey = SignalOutput.kNoneKey

    def BindOscillator (oscillatorKey: Oscillator.Key) = _refs._oscillatorKey = oscillatorKey

    def GetOscillatorKey = _refs._oscillatorKey

    def UnbindOscillator() = _refs._oscillatorKey = Oscillator.kNoneKey

    def BindPeriodOutput(outputKey: SignalOutput.Key) = _refs._periodOutputKey = outputKey

    def GetPeriodOutputKey = _refs._periodOutputKey

    def UnbindPeriodOutput() = _refs._periodOutputKey = SignalOutput.kNoneKey

    def BindPitchOutput(outputKey: SignalOutput.Key) = _refs._pitchOutputKey = outputKey

    def GetPitchOutputKey = _refs._pitchOutputKey

    def UnbindPitchOutput() = _refs._pitchOutputKey = SignalOutput.kNoneKey

    def BindShapeOutput(outputKey: SignalOutput.Key) = _refs._shapeOutputKey = outputKey

    def GetShapeOutputKey = _refs._shapeOutputKey

    def UnbindShapeOutput() = _refs._shapeOutputKey = SignalOutput.kNoneKey

    def BindToneOutput(outputKey: SignalOutput.Key) = _refs._toneOutputKey = outputKey

    def GetToneOutputKey = _refs._toneOutputKey

    def UnbindToneOutput() = _refs._toneOutputKey = SignalOutput.kNoneKey

    def IsTapParent = _refs._isTapParent

    //
    // State:
    //
    protected
    val _state = new OscillatorPatch.State

    def GetEnvelopeReporter (envelopeKey: OscillatorEnvelope.Key): OscillatorPatch.EnvelopeReporter =
        new OscillatorPatch.EnvelopeReporter(_attrs._patchDef, envelopeKey)

    def GetWavesetId = _state._wavesetId

    def ImportEnvelopeDef (envelopeKey: OscillatorEnvelope.Key, envelopeDef: JsObject): Unit =
    {
        // Configure a new patch def with the modified envelope def and import it:
        val newEnvelopeDefs = (_attrs._patchDef \ FieldModel.Glossary.kEnvelopeDef).validate[JsObject] match
        {
            // If envelope defs already exists then append the modified one:
            case JsSuccess (value, _) => value ++ Json.obj(envelopeKey._uniqueKey -> envelopeDef)

            // If no envelope defs yet exists then this will be the first:
            case JsError(errors) => Json.obj(envelopeKey._uniqueKey -> envelopeDef)

        }
        val newPatchDef = _attrs._patchDef ++ Json.obj(FieldModel.Glossary.kEnvelopeDef -> newEnvelopeDefs)
        ImportOscillatorPatchDef(Some(newPatchDef))
    }

    def ImportOscillatorPatchDef (patchDefOpt: Option[JsObject]) =
    {
        val patchDef =
        {
            // Use the patch definition in the attrs propertyset if one is not provided:
            if (patchDefOpt.isDefined)
                _attrs._patchDef = patchDefOpt.get
            _attrs._patchDef
        }

        val wavesetId: String =
            (patchDef \ FieldModel.Glossary.kQWavesetId).validate[String] match
            {
                case JsSuccess(value, _) => value

                case JsError(errors) =>
                    throw new TrunkException(Cell.ErrorCodes.OscillatorPatchInvalid)
            }

        val loudnessEnvelope: Envelope.ContinuousEnvelope =
            (patchDef \ FieldModel.Glossary.kEnvelopeDef \ FieldModel.Glossary.kQLoudness).validate[JsObject] match
            {
                case JsSuccess(value, _) => Envelope.DecodeContinuousEnvelopeDef(value)

                case JsError(errors) =>
                    Envelope.DecodeContinuousEnvelopeDef(Envelope.kTwoThirdsGainWithDeadZoneEnvelopeDef)
            }

        val pitchEnvelope: Envelope.ContinuousEnvelope =
            (patchDef \ FieldModel.Glossary.kEnvelopeDef \ FieldModel.Glossary.kQPitch).validate[JsObject] match
            {
                case JsSuccess(value, _) => Envelope.DecodeContinuousEnvelopeDef(value)

                case JsError(errors) => Envelope.DecodeContinuousEnvelopeDef(Envelope.kUnityGainEnvelopeDef)
            }

        val periodEnvelope: Envelope.DiscreteEnvelope =
            (patchDef \ FieldModel.Glossary.kEnvelopeDef \ FieldModel.Glossary.kQPeriod).validate[JsObject] match
            {
                case JsSuccess(value, _) => Envelope.DecodeDiscreteEnvelopeDef(value)

                case JsError(errors) => Envelope.DecodeDiscreteEnvelopeDef(Envelope.kZeroEnvelopeDef)
            }

        val shapeEnvelope: Envelope.DiscreteEnvelope =
            (patchDef \ FieldModel.Glossary.kEnvelopeDef \ FieldModel.Glossary.kQShape).validate[JsObject] match
            {
                case JsSuccess(value, _) => Envelope.DecodeDiscreteEnvelopeDef(value)

                case JsError(errors) => Envelope.DecodeDiscreteEnvelopeDef(Envelope.kZeroEnvelopeDef)
            }

        val toneEnvelope: Envelope.DiscreteEnvelope =
            (patchDef \ FieldModel.Glossary.kEnvelopeDef \ FieldModel.Glossary.kQTone).validate[JsObject] match
            {
                case JsSuccess(value, _) => Envelope.DecodeDiscreteEnvelopeDef(value)

                case JsError(errors) => Envelope.DecodeDiscreteEnvelopeDef(Envelope.kZeroEnvelopeDef)
            }

        // Commit changes to state propertyset:
        _state._wavesetId = wavesetId
        _state._loudnessEnvelope = loudnessEnvelope
        _state._pitchEnvelope = pitchEnvelope
        _state._periodEnvelope = periodEnvelope
        _state._shapeEnvelope = shapeEnvelope
        _state._toneEnvelope = toneEnvelope
    }

    def MacroSetLoudnessCeiling (ceiling: String) =
    {
        val newEnvelopeDef = (_attrs._patchDef \ FieldModel.Glossary.kEnvelopeDef \
            FieldModel.Glossary.kQLoudness).as[JsObject] ++ Json.obj(FieldModel.Glossary.kEnvelopeCeiling -> ceiling)
        ImportEnvelopeDef(new OscillatorEnvelope.Key(FieldModel.Glossary.kQLoudness), newEnvelopeDef)
    }

    def MacroSetLoudnessPoles (polesPacked: String): Unit =
    {
        val currentEnvelope = _state._loudnessEnvelope
        val newEnvelopeDef = Json.obj(
            FieldModel.Glossary.kEnvelopeCeiling -> currentEnvelope._ceiling.toString,
            FieldModel.Glossary.kEnvelopeFloor -> currentEnvelope._floor.toString,
            FieldModel.Glossary.kEnvelopePolesPackedDef -> polesPacked)
        ImportEnvelopeDef(new OscillatorEnvelope.Key(FieldModel.Glossary.kQLoudness), newEnvelopeDef)
    }

    def MacroSetLoudnessFloor (floor: String)
    {
        val newEnvelopeDef = (_attrs._patchDef \ FieldModel.Glossary.kEnvelopeDef \
            FieldModel.Glossary.kQLoudness).as[JsObject] ++ Json.obj(FieldModel.Glossary.kEnvelopeFloor -> floor)
        ImportEnvelopeDef(new OscillatorEnvelope.Key(FieldModel.Glossary.kQLoudness), newEnvelopeDef)
    }

    def MacroSetPeriodPoles (polesPacked: String)
    {
        val newEnvelopeDef = Json.obj(FieldModel.Glossary.kEnvelopePolesPackedDef -> polesPacked)
        ImportEnvelopeDef(new OscillatorEnvelope.Key(FieldModel.Glossary.kQPeriod), newEnvelopeDef)
    }

    def MacroSetPitchCeiling (ceiling: String)
    {
        val newEnvelopeDef = (_attrs._patchDef \ FieldModel.Glossary.kEnvelopeDef \
            FieldModel.Glossary.kQPitch).as[JsObject] ++ Json.obj(FieldModel.Glossary.kEnvelopeCeiling -> ceiling)
        ImportEnvelopeDef(new OscillatorEnvelope.Key(FieldModel.Glossary.kQPitch), newEnvelopeDef)
    }

    def MacroSetPitchPoles (polesPacked: String)
    {
        val currentEnvelope = _state._pitchEnvelope
        val newEnvelopeDef = Json.obj(
            FieldModel.Glossary.kEnvelopeCeiling -> currentEnvelope._ceiling.toString,
            FieldModel.Glossary.kEnvelopeFloor -> currentEnvelope._floor.toString,
            FieldModel.Glossary.kEnvelopePolesPackedDef -> polesPacked)
        ImportEnvelopeDef(new OscillatorEnvelope.Key(FieldModel.Glossary.kQPitch), newEnvelopeDef)
    }

    def MacroSetPitchFloor (floor: String)
    {
        val newEnvelopeDef = (_attrs._patchDef \ FieldModel.Glossary.kEnvelopeDef \
            FieldModel.Glossary.kQPitch).as[JsObject] ++ Json.obj(FieldModel.Glossary.kEnvelopeFloor -> floor)
        ImportEnvelopeDef(new OscillatorEnvelope.Key(FieldModel.Glossary.kQPitch), newEnvelopeDef)
    }

    def MacroSetShapePoles (polesPacked: String)
    {
        val newEnvelopeDef = Json.obj(FieldModel.Glossary.kEnvelopePolesPackedDef -> polesPacked)
        ImportEnvelopeDef(new OscillatorEnvelope.Key(FieldModel.Glossary.kQShape), newEnvelopeDef)
    }

    def MacroSetTonePoles (polesPacked: String)
    {
        val newEnvelopeDef = Json.obj(FieldModel.Glossary.kEnvelopePolesPackedDef -> polesPacked)
        ImportEnvelopeDef(new OscillatorEnvelope.Key(FieldModel.Glossary.kQTone), newEnvelopeDef)
    }

    def MacroSetWavesetId (wavesetId: String)
    {
        val newPatchDef = _attrs._patchDef.as[JsObject] ++ Json.obj(FieldModel.Glossary.kQWavesetId -> wavesetId)
        ImportOscillatorPatchDef(Some(newPatchDef))
    }

    override
    def Modulate (signal: Signal[_ >: SignalTypes]): SignalModulator.ModulatedSignals =
    {
        val ordinal = signal._ordinal
        val scalar = signal.asInstanceOf[Signal[Continuous]]._scalar

        val modulatedLoudness = Envelope.ModulateWithContinuousEnvelope(scalar, _state._loudnessEnvelope)
        val modulatedPitch = Envelope.ModulateWithContinuousEnvelope(scalar, _state._pitchEnvelope)
        val modulatedPeriod = Envelope.ModulateWithDiscreteEnvelope(scalar, _state._periodEnvelope)
        val modulatedShape = Envelope.ModulateWithDiscreteEnvelope(scalar, _state._shapeEnvelope)
        val modulatedTone = Envelope.ModulateWithDiscreteEnvelope(scalar, _state._toneEnvelope)
        SignalModulator.ModulatedSignals(
            List(
                (Some(FieldModel.Glossary.kQLoudness), Signal[Continuous](ordinal, modulatedLoudness, Some(GetKey))),
                (Some(FieldModel.Glossary.kQPitch), Signal[Continuous](ordinal, modulatedPitch, Some(GetKey))),
                (Some(FieldModel.Glossary.kQPeriod), Signal[Discrete](ordinal, modulatedPeriod, Some(GetKey))),
                (Some(FieldModel.Glossary.kQShape), Signal[Discrete](ordinal, modulatedShape, Some(GetKey))),
                (Some(FieldModel.Glossary.kQTone), Signal[Discrete](ordinal, modulatedTone, Some(GetKey)))))
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
                    new SignalTap.Query(Vector(_refs._tapKey), sectionsOpt)))
        }
        if (sections.HasChildReports)
            report ++=
                Json.obj(TrunkModel.Glossary.kRSignalOutputs ->
                    Json.obj(
                        FieldModel.Glossary.kQLoudness -> _trunkModel.ReportSignalOutputs(
                            GetTrunkKey,
                            new SignalOutput.Query(Vector(_refs._loudnessOutputKey), sectionsOpt)).head,
                        FieldModel.Glossary.kQPitch -> _trunkModel.ReportSignalOutputs(
                            GetTrunkKey,
                            new SignalOutput.Query(Vector(_refs._pitchOutputKey), sectionsOpt)).head,
                        FieldModel.Glossary.kQPeriod -> _trunkModel.ReportSignalOutputs(
                            GetTrunkKey,
                            new SignalOutput.Query(Vector(_refs._periodOutputKey), sectionsOpt)).head,
                        FieldModel.Glossary.kQShape -> _trunkModel.ReportSignalOutputs(
                            GetTrunkKey,
                            new SignalOutput.Query(Vector(_refs._shapeOutputKey), sectionsOpt)).head,
                        FieldModel.Glossary.kQTone -> _trunkModel.ReportSignalOutputs(
                            GetTrunkKey,
                            new SignalOutput.Query(Vector(_refs._toneOutputKey), sectionsOpt)).head))

        report
    }

    def TestSignal (signal: Signal[_ >: SignalTypes]) =
    {
        _trunkModel.GetSignalTapOpt(GetTrunkKey, _refs._tapKey) match
        {
            case Some(tap) => tap.Propagate(Some(signal))

            case None => throw new TrunkException(Cell.ErrorCodes.OscillatorPatchTapless)
        }
    }

    override
    def Trigger () =
    {
        _trunkModel.GetSignalTapOpt(GetTrunkKey, _refs._tapKey) match
        {
            case Some(tap) => tap.Propagate()

            case None => throw new TrunkException(Cell.ErrorCodes.OscillatorPatchTapless)
        }
    }


    // Import initial patch definition:
    _logger.LogDebug(s"OP: importing oscillator patch definition")
    ImportOscillatorPatchDef(None)
}
