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
import org.taranos.mc.trunk.intraprocess.BiasedElement.{BiasedConstructorMetaDecoder, BiasedUpdateMetaDecoder}
import org.taranos.mc.trunk.intraprocess.Signal.SignalTypes
import org.taranos.mc.trunk.intraprocess.TestableElement.TestableUpdateStateDecoder
import org.taranos.mc.trunk.intraprocess.TrunkElement.{CommonConstructorMetaDecoder, CommonDestructorMetaDecoder, CommonQueryDecoder, CommonUpdateMetaDecoder}
import play.api.libs.json.{JsError, JsObject, JsSuccess, Json}


object SignalTap
{
    class Key (uniqueKey: TrunkElement.UniqueKey, symbol: Symbol = 'SignalTap)
        extends SignalPath.Key(uniqueKey, symbol)
            with TappableElementKey

    class Meta (
        uniqueKey: TrunkElement.UniqueKey,
        tag: String,
        badgeOpt: Option[String],
        nameOpt: Option[String],
        descriptionOpt: Option[String],
        mode: Signal.ModeEnum.Mode)
        extends SignalPath.Meta[SignalTap.Key](
            new SignalTap.Key(uniqueKey),
            tag,
            badgeOpt,
            nameOpt,
            descriptionOpt,
            mode)

    class Attrs
        extends SignalPath.Attrs

    class Refs (
        trunkKey: Trunk.Key,
        sinkKey: SignalSink.Key,
        sourceKey: SignalSource.Key,
        var _modulatorKeyOpt: Option[SignalModulator.Key])
        extends SignalPath.Refs(
            trunkKey,
            sinkKey,
            sourceKey,
            _isTap = true)
    {
        override
        def Report (sections: ReportSectionsParser): JsObject =
        {
            var report = super.Report(sections)

            if (!sections.HasPeerReports)
            {
                val modulatorKeyEncoded = _modulatorKeyOpt match
                {
                    case Some(modulatorKey) => TrunkElement.EncodeKey(modulatorKey)

                    case None => ""
                }
                report ++=
                    Json.obj (TrunkModel.Glossary.kESignalModulator -> modulatorKeyEncoded)
            }

            report
        }
    }

    class State
        extends SignalPath.State

    case class Constructor (
        _tag: String,
        _badgeOpt: Option[String] = None,
        _nameOpt: Option[String] = None,
        _descriptionOpt: Option[String] = None,
        _mode: Signal.ModeEnum.Mode,
//        // Endpoint-pair binding:
//        _endpointPairOpt: Option[SignalEndpointPair] = None,
        // TappableElement binding:
        _tappableKeyOpt: Option[TrunkElement.Key] = None,
        _modulatorKeyOpt: Option[SignalModulator.Key] = None)

    case class Destructor (
        _key: SignalTap.Key)

    case class Query (
        keys: Vector[SignalTap.Key],
        sectionsOpt: Option[String] = None)
        extends TrunkElement.Query[SignalTap.Key](
            keys,
            sectionsOpt)

    case class Update (
        _key: SignalTap.Key,
        _nameOpt: Option[String],
        _descriptionOpt: Option[String],
        _modeOpt: Option[Signal.ModeEnum.Mode],
        _signalEncodedOpt: Option[String])

    val kAnyKey = new Key(TrunkModel.Glossary.kAnyKeyBase)

    val kNoneKey = new Key(TrunkModel.Glossary.kNoneKeyBase)

    def DecodeConstructor (encoded: String): Constructor =
    {
        val constructor = Json.parse(encoded)

        val commonMeta = new CommonConstructorMetaDecoder(constructor, Cell.ErrorCodes.SignalTapConstructorInvalid)

        val biasedMeta = new BiasedConstructorMetaDecoder(constructor)
        val mode = biasedMeta._modeOpt.getOrElse(Signal.ModeEnum.Unbiased)

        val tappableKeyOpt: Option[TrunkElement.Key] =
            (constructor \ TrunkModel.Glossary.kESignalTap).validate[String] match
            {
                case JsSuccess(value, _) => Some(TrunkElement.DecodeKey[TrunkElement.Key](value))
                case JsError(errors) => None
            }

        new Constructor(
            commonMeta._tag,
            commonMeta._badgeOpt,
            commonMeta._nameOpt,
            commonMeta._descriptionOpt,
            mode,
            _tappableKeyOpt = tappableKeyOpt)
    }

    def DecodeDestructor (encoded: String): Destructor =
    {
        val destructor = Json.parse(encoded)

        val commonMeta = new CommonDestructorMetaDecoder[SignalTap.Key](
            destructor, Cell.ErrorCodes.SignalTapDestructorInvalid)

        new Destructor(commonMeta._key)
    }

    def DecodeQuery (encoded: String): Query =
    {
        val query = Json.parse(encoded)

        val commonQuery = new CommonQueryDecoder[SignalTap.Key](query)

        new Query(commonQuery._keysOpt.get, commonQuery._sectionsOpt)
    }

    def DecodeUpdate (encoded: String): Update =
    {
        val update = Json.parse(encoded)

        val commonMeta = new CommonUpdateMetaDecoder[SignalTap.Key](
            update, Cell.ErrorCodes.SignalTapUpdateInvalid)

        val biasedMeta = new BiasedUpdateMetaDecoder(update)

        val testableState = new TestableUpdateStateDecoder(update)

        new Update(
            commonMeta._key,
            commonMeta._nameOpt,
            commonMeta._descriptionOpt,
            biasedMeta._modeOpt,
            testableState._signalEncodedOpt)
    }
}

class SignalTap (
    meta: SignalTap.Meta,
    attrs: SignalTap.Attrs,
    refs: SignalTap.Refs)
    (implicit protected val _trunkModel: TrunkModel)
    extends SignalPath[SignalTap.Key]
        with ListenerElement
        with TappableElement
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

    //
    // Refs:
    //
    protected
    val _refs = refs

    def BindModulator (
        modulatorKey: SignalModulator.Key,
        isListener: Boolean) =
    {
        _refs._modulatorKeyOpt = Some(modulatorKey)

        if (isListener)
        {
            _trunkModel.GetSignalSinkOpt(GetTrunkKey, _refs._sinkKey) match
            {
                case Some(sink) => sink.SetListenerOpt(Some(this))

                case None => throw new TrunkException(Cell.ErrorCodes.SignalTapSinkless)
            }
        }
    }

    def BindSink (
        sinkKey: SignalSink.Key,
        isReciprocal: Boolean): Unit =
    {
        _refs._sinkKey = sinkKey

        if (isReciprocal)
        {
            _trunkModel.GetSignalSinkOpt(GetTrunkKey, _refs._sinkKey) match
            {
                case Some(sink) => sink.BindTap(GetKey, isReciprocal = false)

                case None => throw new TrunkException(Cell.ErrorCodes.SignalSinkInvalid)
            }
        }
    }

    def BindSource (
        sourceKey: SignalSource.Key,
        isReciprocal: Boolean): Unit =
    {
        _refs._sourceKey = sourceKey
        if (isReciprocal)
        {
            _trunkModel.GetSignalSourceOpt(GetTrunkKey, _refs._sourceKey) match
            {
                case Some(source) => source.BindTap(GetKey, isReciprocal = false)

                case None => throw new TrunkException(Cell.ErrorCodes.SignalSourceInvalid)
            }
        }
    }

    def GetModulatorKeyOpt: Option[SignalModulator.Key] = _refs._modulatorKeyOpt

    def GetSinkKey: SignalSink.Key = _refs._sinkKey

    def GetSourceKey: SignalSource.Key = _refs._sourceKey

    def UnbindModulator () = _refs._modulatorKeyOpt = None

    def UnbindSink (isReciprocal: Boolean): Unit =
    {
        if (_refs._sinkKey != SignalSink.kNoneKey && isReciprocal)
        {
            _trunkModel.GetSignalSinkOpt(GetTrunkKey, _refs._sinkKey) match
            {
                case Some(sink) => sink.UnbindTap(isReciprocal = false)

                case None =>    // We don't care...
            }
        }

        _refs._sinkKey = SignalSink.kNoneKey
    }

    def UnbindSource (isReciprocal: Boolean): Unit =
    {
        if (_refs._sourceKey != SignalSource.kNoneKey && isReciprocal)
        {
            _trunkModel.GetSignalSourceOpt(GetTrunkKey, _refs._sourceKey) match
            {
                case Some(source) => source.UnbindTap(isReciprocal = false)

                case None =>    // We don't care...
            }
        }

        _refs._sourceKey = SignalSource.kNoneKey
    }

    //
    // State:
    //
    protected
    val _state = null

    def GetLastSignalOpt: Option[Signal[_ >: SignalTypes]] =
    {
        _trunkModel.GetSignalSinkOpt(GetTrunkKey, _refs._sinkKey) match
        {
            case Some(sink) => sink.GetLastTrappedSignalOpt

            case None => throw new TrunkException(Cell.ErrorCodes.SignalTapSinkless)
        }
    }

    def GetSignals: Iterable[Signal[_ >: SignalTypes]] =
    {
        _trunkModel.GetSignalSinkOpt(GetTrunkKey, _refs._sinkKey) match
        {
            case Some(sink) => sink.GetTrappedSignals

            case None => throw new TrunkException(Cell.ErrorCodes.SignalTapSinkless)
        }
    }

    def Notify (propagatorKey: TrunkElement.Key): Unit =
    {
        _refs._modulatorKeyOpt match
        {
            case Some(modulatorKey) =>
                val modulator = SignalModulator.GetModulator(
                    _trunkModel,
                    GetTrunkKey,
                    modulatorKey)
                modulator.Trigger()

            case None =>
                Propagate()
        }
    }

    def Propagate (
        signalOpt: Option[Signal[_ >: SignalTypes]],
        partOpt: Option[String]): Unit =
    {
        def PassThrough (signalOpt: Option[Signal[_ >: SignalTypes]]) =
            SignalModulator.ModulatedSignals(
                signalOpt match
                {
                    case Some(signal) => List((None, signal))

                    case _ => List()
                })

        // If tap is injecting the signal:
        if (signalOpt.isDefined)
        {
            val signal = signalOpt.get

            // Attempt to modulate signal:
            val modulatedSignals: SignalModulator.ModulatedSignals =
                // If signal is virtual then accept its mark and bypass normal modulation:
                if (signal._scalar.isInstanceOf[Signal.Virtual])
                {
                    SetMark(signal._ordinal)

                    SignalModulator.ModulatedSignals(
                        List(
                            (None, signal)))
                }
                else
                    _refs._modulatorKeyOpt match
                    {
                        case Some(modulatorKey) =>
                            modulatorKey match
                            {
                                // If signal was injected from bridge then pass it through:
                                case _: SignalBridge.Key =>
                                    PassThrough(signalOpt)

                                // Forward signal to signal input:
                                case inputKey: SignalInput.Key =>
                                    _trunkModel.GetSignalInputOpt(GetTrunkKey, inputKey) match
                                    {
                                        case Some(input) => input.Modulate(signal)

                                        case None => throw new TrunkException(Cell.ErrorCodes.SignalTapModulatorless)
                                    }

                                // Forward signal to emitter patch:
                                case emitterPatchKey: EmitterPatch.Key =>
                                    _trunkModel.GetEmitterPatchOpt(GetTrunkKey, emitterPatchKey) match
                                    {
                                        case Some(emitterPatch) => emitterPatch.Modulate(signal)

                                        case None => throw new TrunkException(Cell.ErrorCodes.SignalTapModulatorless)
                                    }

                                // Forward signal to oscillator patch:
                                case oscillatorPatchKey: OscillatorPatch.Key =>
                                    _trunkModel.GetOscillatorPatchOpt(GetTrunkKey, oscillatorPatchKey) match
                                    {
                                        case Some(oscillatorPatch) => oscillatorPatch.Modulate(signal)

                                        case None => throw new TrunkException(Cell.ErrorCodes.SignalTapModulatorless)
                                    }

                                // Anything else provides no modulation:
                                case _ =>
                                    SignalModulator.ModulatedSignals()
                            }

                        case None =>
                            // If no modulator is inserted then pass signal through:
                            PassThrough(signalOpt)
                    }

            // Forward modulated signals to child source:
            modulatedSignals._signals.foreach(modulatedSignal =>
            {
                import scala.language.existentials

                val (forwardPartOpt, forwardSignal) = modulatedSignal

                // There must be a valid source bound with the tap:
                val source = _trunkModel.GetSignalSourceOpt(GetTrunkKey, _refs._sourceKey).getOrElse(
                    throw new TrunkException(Cell.ErrorCodes.SignalTapSourceless))

                forwardSignal._propagatorKeyOpt = Some(GetKey)
                _trunkModel.Log( s"$GetTag propagating ${forwardSignal.ToFriendly}")
                source.Propagate(Some(forwardSignal), forwardPartOpt)
            })
        }
        // Else if performing default propagation:
        else
        {
            _trunkModel.GetSignalSinkOpt(GetTrunkKey, _refs._sinkKey) match
            {
                case Some(sink) =>
                    val signalOpt = sink.GetLastTrappedSignalOpt
                    if (signalOpt.isDefined)
                        Propagate(signalOpt)

                case None => TrunkException(Cell.ErrorCodes.SignalTapSinkless)
            }
        }
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

        // Add child reports:
        if (sections.HasChildReports)
        {
            if (_refs._sinkKey != SignalSink.kNoneKey)
                report ++=
                    Json.obj(TrunkModel.Glossary.kRSignalSinks -> _trunkModel.ReportSignalSinks(
                        GetTrunkKey,
                        new SignalSink.Query(Vector(_refs._sinkKey), sectionsOpt)))

            if (_refs._sourceKey != SignalSource.kNoneKey)
                report ++=
                    Json.obj(TrunkModel.Glossary.kRSignalSources -> _trunkModel.ReportSignalSources(
                        GetTrunkKey,
                        new SignalSource.Query(Vector(_refs._sourceKey), sectionsOpt)))
        }

        report
    }

    def TestSignal (signal: Signal[_ >: SignalTypes]) =
    {
        Propagate(Some(signal))
    }


    // Automatically bind with endpoints:
    BindSink(_refs._sinkKey, isReciprocal = true)
    BindSource(_refs._sourceKey, isReciprocal = true)
}
