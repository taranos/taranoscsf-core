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
import org.taranos.mc.Common.ReportSectionsParser
import org.taranos.mc.trunk.intraprocess.BiasedElement.{BiasedConstructorMetaDecoder, BiasedUpdateMetaDecoder}
import org.taranos.mc.trunk.intraprocess.Signal.SignalTypes
import org.taranos.mc.trunk.intraprocess.TestableElement.TestableUpdateStateDecoder
import org.taranos.mc.trunk.intraprocess.TrunkElement.{CommonConstructorMetaDecoder, CommonDestructorMetaDecoder,
    CommonQueryDecoder, CommonUpdateMetaDecoder}
import play.api.libs.json.{JsError, JsObject, JsSuccess, Json}


object SignalLink
{
    class Key (uniqueKey: TrunkElement.UniqueKey, symbol: Symbol = 'SignalLink)
        extends SignalPath.Key(uniqueKey, symbol)

    class Meta (
        uniqueKey: TrunkElement.UniqueKey,
        tag: String,
        badgeOpt: Option[String],
        nameOpt: Option[String],
        descriptionOpt: Option[String],
        mode: Signal.ModeEnum.Mode = Signal.ModeEnum.Unbiased)
        extends SignalPath.Meta[SignalLink.Key](
            new SignalLink.Key(uniqueKey),
            tag,
            badgeOpt,
            nameOpt,
            descriptionOpt,
            mode)

    class Attrs (
        var _partOpt: Option[String])
        extends SignalPath.Attrs
    {
        override
        def Report (sections: ReportSectionsParser): JsObject =
        {
            var report = super.Report(sections)

            if (_partOpt.isDefined)
                report ++= Json.obj(TrunkModel.Glossary.kPart -> _partOpt.get)

            report
        }
    }

    class Refs (
        trunkKey: Trunk.Key,
        sinkKey: SignalSink.Key,
        sourceKey: SignalSource.Key)
        extends SignalPath.Refs(
            trunkKey,
            sinkKey,
            sourceKey,
            _isTap = false)

    class State
        extends SignalPath.State

    case class Constructor (
        _tag: String,
        _badgeOpt: Option[String] = None,
        _nameOpt: Option[String] = None,
        _descriptionOpt: Option[String] = None,
        _mode: Signal.ModeEnum.Mode = Signal.ModeEnum.Unbiased,
        _sourceKey: SignalSource.Key,
        _sinkKey: SignalSink.Key,
        _partOpt: Option[String] = None)

    case class Destructor (
        _key: SignalLink.Key)

    case class Query (
        keys: Vector[SignalLink.Key],
        sectionsOpt: Option[String] = None)
        extends TrunkElement.Query[SignalLink.Key](
            keys,
            sectionsOpt)

    case class Update (
        _key: SignalLink.Key,
        _nameOpt: Option[String],
        _descriptionOpt: Option[String],
        _modeOpt: Option[Signal.ModeEnum.Mode],
        _signalEncodedOpt: Option[String])

    val kAnyKey = new Key(TrunkModel.Glossary.kAnyKeyBase)

    def DecodeConstructor (encoded: String): Constructor =
    {
        val constructor = Json.parse(encoded)

        val commonMeta = new CommonConstructorMetaDecoder(constructor, Cell.ErrorCodes.SignalLinkConstructorInvalid)

        val biasedMeta = new BiasedConstructorMetaDecoder(constructor)
        val mode = biasedMeta._modeOpt.getOrElse(Signal.ModeEnum.Unbiased)

        val sourceKey: SignalSource.Key =
            (constructor \ TrunkModel.Glossary.kPSRefs \ TrunkModel.Glossary.kESignalSource).validate[String] match
            {
                case JsSuccess(value, _) => TrunkElement.DecodeKey[SignalSource.Key](value)

                case JsError(_) =>
                    throw TrunkException(
                        Cell.ErrorCodes.SignalLinkConstructorInvalid,
                        "missing source key property")
            }

        val sinkKey: SignalSink.Key =
            (constructor \ TrunkModel.Glossary.kPSRefs \ TrunkModel.Glossary.kESignalSink).validate[String] match
            {
                case JsSuccess(value, _) => TrunkElement.DecodeKey[SignalSink.Key](value)

                case JsError(_) =>
                    throw TrunkException(
                        Cell.ErrorCodes.SignalLinkConstructorInvalid,
                        "missing sink key property")
            }

        val partOpt: Option[String] =
            (constructor \ TrunkModel.Glossary.kPSAttrs \ TrunkModel.Glossary.kPart).validate[String] match
            {
                case JsSuccess(value, _) => Some(value)

                case JsError(_) => None
            }

        Constructor(
            commonMeta._tag,
            commonMeta._badgeOpt,
            commonMeta._nameOpt,
            commonMeta._descriptionOpt,
            mode,
            sourceKey,
            sinkKey,
            partOpt)
    }

    def DecodeDestructor (encoded: String): Destructor =
    {
        val destructor = Json.parse(encoded)

        val commonMeta = new CommonDestructorMetaDecoder[SignalLink.Key](
            destructor, Cell.ErrorCodes.SignalLinkDestructorInvalid)

        Destructor(commonMeta._key)
    }

    def DecodeQuery (encoded: String): Query =
    {
        val query = Json.parse(encoded)

        val commonQuery = new CommonQueryDecoder[SignalLink.Key](query)

        Query(commonQuery._keysOpt.get, commonQuery._sectionsOpt)
    }

    def DecodeUpdate (encoded: String): Update =
    {
        val update = Json.parse(encoded)

        val commonMeta = new CommonUpdateMetaDecoder[SignalLink.Key](
            update, Cell.ErrorCodes.SignalLinkUpdateInvalid)

        val biasedMeta = new BiasedUpdateMetaDecoder(update)

        val testableState = new TestableUpdateStateDecoder(update)

        Update(
            commonMeta._key,
            commonMeta._nameOpt,
            commonMeta._descriptionOpt,
            biasedMeta._modeOpt,
            testableState._signalEncodedOpt)
    }
}

class SignalLink (
    meta: SignalLink.Meta,
    attrs: SignalLink.Attrs,
    refs: SignalLink.Refs)
    (implicit protected val _trunkModel: TrunkModel)
    extends SignalPath[SignalLink.Key]
{
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

    def BindSink (
        sinkKey: SignalSink.Key,
        isReciprocal: Boolean): Unit =
    {
        _refs._sinkKey = sinkKey

        if (isReciprocal)
        {
            _trunkModel.GetSignalSinkOpt(GetTrunkKey, _refs._sinkKey) match
            {
                case Some(sink) =>
                    sink.BindLink(
                        GetKey,
                        _attrs._partOpt.getOrElse(TrunkElement.EncodeKey(GetKey)),
                        isReciprocal = false)

                case None => throw TrunkException(Cell.ErrorCodes.SignalSinkInvalid)
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
            _trunkModel.GetSignalSourceOpt(GetTrunkKey, sourceKey) match
            {
                case Some(source) =>
                    source.BindLink(
                        GetKey,
                        _attrs._partOpt.getOrElse(TrunkElement.EncodeKey(GetKey)),
                        isReciprocal = false)

                case None => throw TrunkException(Cell.ErrorCodes.SignalSourceInvalid)
            }
        }
    }

    def GetSinkKey: SignalSink.Key =
        _refs._sinkKey

    def GetSourceKey: SignalSource.Key =
        _refs._sourceKey

    def UnbindSink (isReciprocal: Boolean): Unit =
    {
        if (_refs._sinkKey != SignalSink.kNoneKey && isReciprocal)
        {
            _trunkModel.GetSignalSinkOpt(GetTrunkKey, _refs._sinkKey) match
            {
                case Some(sink) => sink.UnbindLink(Some(GetKey), isReciprocal = false)

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
                case Some(source) => source.UnbindLink(Some(GetKey), isReciprocal = false)

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

    def Propagate (
        signalOpt: Option[Signal[_ >: SignalTypes]],
        partOpt: Option[String]): Unit =
    {
        // Link must be passing-through a signal (links do not perform default propagation):
        val signal = signalOpt.getOrElse(
            throw TrunkException(Cell.ErrorCodes.SignalInvalid, "signal must be valid"))

        // If signal is virtual then accept its mark:
        if (signal._scalar.isInstanceOf[Signal.Virtual])
            SetMark(signal._ordinal)

        // There must be a valid sink bound with the link:
        val sink = _trunkModel.GetSignalSinkOpt(GetTrunkKey, _refs._sinkKey).getOrElse(
            throw TrunkException(Cell.ErrorCodes.SignalLinkSinkless))

        // Forward signal to child sink:
        signal._propagatorKeyOpt = Some(GetKey)
        _trunkModel.Log(s"$GetTag propagating ${signal.ToFriendly} to ${sink.GetTag}")
        sink.Propagate(Some(signal))
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
            report ++=
                Json.obj(TrunkModel.Glossary.kRSignalSinks -> _trunkModel.ReportSignalSinks(
                    GetTrunkKey,
                    SignalSink.Query(Vector(_refs._sinkKey), sectionsOpt)))

            report ++=
                Json.obj(TrunkModel.Glossary.kRSignalSources -> _trunkModel.ReportSignalSources(
                    GetTrunkKey,
                    SignalSource.Query(Vector(_refs._sourceKey), sectionsOpt)))
        }

        report
    }

    def PropagateTest (signal: Signal[_ >: SignalTypes]): Unit =
    {
        Propagate(Some(signal))
    }


    // Automatically bind with endpoints:
    BindSink(refs._sinkKey, isReciprocal = true)
    BindSource(refs._sourceKey, isReciprocal = true)
}
