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
import org.taranos.mc.trunk.intraprocess.BiasedElement.BiasedConstructorMetaDecoder
import org.taranos.mc.trunk.intraprocess.Signal.SignalTypes
import org.taranos.mc.trunk.intraprocess.TestableElement.TestableUpdateStateDecoder
import org.taranos.mc.trunk.intraprocess.TrunkElement.{CommonConstructorMetaDecoder, CommonDestructorMetaDecoder, CommonQueryDecoder, CommonUpdateMetaDecoder}
import play.api.libs.json.{JsError, JsObject, JsSuccess, Json}


object SignalOutput
{
    class Key (uniqueKey: TrunkElement.UniqueKey, symbol: Symbol = 'SignalOutput)
        extends SignalModulator.Key(uniqueKey, symbol)

    class Meta (
        uniqueKey: TrunkElement.UniqueKey,
        tag: String,
        badgeOpt: Option[String],
        nameOpt: Option[String],
        descriptionOpt: Option[String],
        mode: Signal.ModeEnum.Mode = Signal.ModeEnum.Unbiased)
        extends SignalModulator.Meta[SignalOutput.Key](
            new SignalOutput.Key(uniqueKey),
            tag,
            badgeOpt,
            nameOpt,
            descriptionOpt,
            mode)

    class Attrs
        extends SignalModulator.Attrs

    class Refs (
        trunkKey: Trunk.Key,
        tapKey: SignalTap.Key,
        isTapParent: Boolean)
        extends SignalModulator.Refs(
            trunkKey,
            tapKey,
            isTapParent)

    class State
        extends SignalModulator.State

    case class Constructor (
        _tag: String,
        _badgeOpt: Option[String] = None,
        _nameOpt: Option[String] = None,
        _descriptionOpt: Option[String] = None,
        _mode: Signal.ModeEnum.Mode,
        _modulatableKey: ModulatableElementKey,
        _tappableKeyOpt: Option[TappableElementKey],
        _partOpt: Option[String] = None)

    case class Destructor (
        _key: SignalOutput.Key)

    case class Query (
        keys: Vector[SignalOutput.Key],
        sectionsOpt: Option[String] = None)
        extends TrunkElement.Query[SignalOutput.Key](
            keys,
            sectionsOpt)

    case class Update (
        _key: SignalOutput.Key,
        _nameOpt: Option[String],
        _descriptionOpt: Option[String],
        _signalEncodedOpt: Option[String])

    val kAnyKey = new Key(TrunkModel.Glossary.kAnyKeyBase)

    val kNoneKey = new Key(TrunkModel.Glossary.kNoneKeyBase)

    def DecodeConstructor (encoded: String): Constructor =
    {
        val constructor = Json.parse(encoded)

        val commonMeta = new CommonConstructorMetaDecoder(constructor, Cell.ErrorCodes.SignalOutputConstructorInvalid)

        val biasedMeta = new BiasedConstructorMetaDecoder(constructor)
        val mode = biasedMeta._modeOpt.getOrElse(Signal.ModeEnum.Unbiased)

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
                                Cell.ErrorCodes.SignalOutputConstructorInvalid,
                                "invalid tappable key property")
                    }

                case JsError(errors) => None
            }

        val modulatableKey: ModulatableElementKey =
            (constructor \ TrunkModel.Glossary.kPSRefs \ TrunkModel.Glossary.kESignalModulator).validate[String] match
            {
                case JsSuccess(value, _) =>
                    val key = TrunkElement.DecodeKey[TrunkElement.Key](value)
                    key match

                    {
                        case _: ModulatableElementKey => key.asInstanceOf[ModulatableElementKey]

                        case _ =>
                            throw new TrunkException(
                                Cell.ErrorCodes.SignalOutputConstructorInvalid,
                                "invalid modulatable key property")
                    }

                case JsError(errors) =>
                    throw new TrunkException(
                        Cell.ErrorCodes.SignalOutputConstructorInvalid,
                        "missing modulatable key property")
            }

        val partOpt: Option[String] =
            (constructor \ TrunkModel.Glossary.kPSAttrs \ TrunkModel.Glossary.kPart).validate[String] match
            {
                case JsSuccess(value, _) => Some(value)

                case JsError(errors) => None
            }

        new Constructor(
            commonMeta._tag,
            commonMeta._badgeOpt,
            commonMeta._nameOpt,
            commonMeta._descriptionOpt,
            mode,
            modulatableKey,
            tappableKeyOpt,
            partOpt)
    }

    def DecodeDestructor (encoded: String): Destructor =
    {
        val destructor = Json.parse(encoded)

        val commonMeta = new CommonDestructorMetaDecoder[SignalOutput.Key](
            destructor, Cell.ErrorCodes.SignalOutputDestructorInvalid)

        new Destructor(commonMeta._key)
    }

    def DecodeQuery (encoded: String): Query =
    {
        val query = Json.parse(encoded)

        val commonQuery = new CommonQueryDecoder[SignalOutput.Key](query)

        new Query(commonQuery._keysOpt.get, commonQuery._sectionsOpt)
    }

    def DecodeUpdate (encoded: String): Update =
    {
        val update = Json.parse(encoded)

        val commonMeta = new CommonUpdateMetaDecoder[SignalOutput.Key](
            update, Cell.ErrorCodes.SignalOutputUpdateInvalid)

        val testableState = new TestableUpdateStateDecoder(update)

        new Update(
            commonMeta._key,
            commonMeta._nameOpt,
            commonMeta._descriptionOpt,
            testableState._signalEncodedOpt)
    }
}

class SignalOutput (
    meta: SignalOutput.Meta,
    attrs: SignalOutput.Attrs,
    refs: SignalOutput.Refs)
    (implicit protected val _trunkModel: TrunkModel)
    extends SignalModulator[SignalOutput.Key]
        with EgressElement
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

    def GetTapKey: SignalTap.Key = _refs._tapKey

    //
    // State:
    //
    protected
    val _state = null

    def GetSignalOpt: Option[Signal[_ >: SignalTypes]] =
    {
        _trunkModel.GetSignalTapOpt(GetTrunkKey, _refs._tapKey) match
        {
            case Some(tap) => tap.GetLastSignalOpt

            case None => throw new TrunkException(Cell.ErrorCodes.SignalOutputTapless)
        }
    }

    def IsTapParent = _refs._isTapParent

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
            report ++= Json.obj(TrunkModel.Glossary.kPSState ->
                Json.obj(TrunkModel.Glossary.kESignal ->
                    {
                        GetSignalOpt match
                        {
                            case Some(signal) => signal.Report(sections)

                            case None => ""
                        }
                    }))

        // Add peer reports:
        if (sections.HasPeerReports)
        {
            report ++=
                Json.obj(TrunkModel.Glossary.kRSignalTaps -> _trunkModel.ReportSignalTaps(
                    GetTrunkKey,
                    new SignalTap.Query(Vector(_refs._tapKey), sectionsOpt)))
        }

        report
    }

    def TestSignal (signal: Signal[_ >: SignalTypes]) =
    {
        _trunkModel.GetSignalTapOpt(GetTrunkKey, _refs._tapKey) match
        {
            case Some(tap) => tap.Propagate(Some(signal))

            case None => throw new TrunkException(Cell.ErrorCodes.SignalOutputTapless)
        }
    }
}
