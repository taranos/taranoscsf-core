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
import org.taranos.mc.trunk.intraprocess.TrunkElement.{CommonConstructorMetaDecoder, CommonDestructorMetaDecoder,
    CommonQueryDecoder, CommonUpdateMetaDecoder}
import play.api.libs.json.{JsError, JsObject, JsSuccess, Json}


object SignalInput
{
    class Key (uniqueKey: TrunkElement.UniqueKey, symbol: Symbol = 'SignalInput)
        extends SignalModulator.Key(uniqueKey, symbol)

    class Meta (
        uniqueKey: TrunkElement.UniqueKey,
        tag: String,
        badgeOpt: Option[String],
        nameOpt: Option[String],
        descriptionOpt: Option[String],
        mode: Signal.ModeEnum.Mode = Signal.ModeEnum.Unbiased)
        extends SignalModulator.Meta[SignalInput.Key](
            new SignalInput.Key(uniqueKey),
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
        _tappableKeyOpt: Option[TappableElementKey] = None)

    case class Destructor (
        _key: SignalInput.Key)

    case class Query (
        keys: Vector[SignalInput.Key],
        sectionsOpt: Option[String] = None)
        extends TrunkElement.Query[SignalInput.Key](
            keys,
            sectionsOpt)

    case class Update (
        _key: SignalInput.Key,
        _nameOpt: Option[String],
        _descriptionOpt: Option[String],
        _signalEncodedOpt: Option[String])

    val kAnyKey = new Key(TrunkModel.Glossary.kAnyKeyBase)

    def DecodeConstructor (encoded: String): Constructor =
    {
        val constructor = Json.parse(encoded)

        val commonMeta = new CommonConstructorMetaDecoder(constructor, Cell.ErrorCodes.SignalInputConstructorInvalid)

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
                            throw TrunkException(
                                Cell.ErrorCodes.SignalInputConstructorInvalid,
                                "invalid tappable key property")
                    }

                case JsError(_) => None
            }

        Constructor(
            commonMeta._tag,
            commonMeta._badgeOpt,
            commonMeta._nameOpt,
            commonMeta._descriptionOpt,
            mode,
            tappableKeyOpt)
    }

    def DecodeDestructor (encoded: String): Destructor =
    {
        val destructor = Json.parse(encoded)

        val commonMeta = new CommonDestructorMetaDecoder[SignalInput.Key](
            destructor, Cell.ErrorCodes.SignalInputDestructorInvalid)

        Destructor(commonMeta._key)
    }

    def DecodeQuery (encoded: String): Query =
    {
        val query = Json.parse(encoded)

        val commonQuery = new CommonQueryDecoder[SignalInput.Key](query)

        Query(commonQuery._keysOpt.get, commonQuery._sectionsOpt)
    }

    def DecodeUpdate (encoded: String): Update =
    {
        val update = Json.parse(encoded)

        val commonMeta = new CommonUpdateMetaDecoder[SignalInput.Key](
            update, Cell.ErrorCodes.SignalInputUpdateInvalid)

        val testableState = new TestableUpdateStateDecoder(update)

        Update(
            commonMeta._key,
            commonMeta._nameOpt,
            commonMeta._descriptionOpt,
            testableState._signalEncodedOpt)
    }
}

class SignalInput (
    meta: SignalInput.Meta,
    attrs: SignalInput.Attrs,
    refs: SignalInput.Refs)
    (implicit protected val _trunkModel: TrunkModel)
    extends SignalModulator[SignalInput.Key]
        with IngressElement
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

    def GetTapKey: SignalTap.Key =
        _refs._tapKey

    //
    // State:
    //
    protected
    val _state = null

    def IsTapParent: Boolean =
        _refs._isTapParent

    override
    def Modulate (signal: Signal[_ >: SignalTypes]): ModulatableElement.ModulatedSignals =
    {
        // Pass-through signal unmodulated:
        signal._propagatorKeyOpt = Some(GetKey)
        ModulatableElement.ModulatedSignals(
            List(
                (None, signal)))
    }

    def PutSignal (signal: Signal[_ >: SignalTypes]): Unit =
    {
        _trunkModel.GetSignalTapOpt(GetTrunkKey, _refs._tapKey) match
        {
            case Some(tap) => tap.Propagate(Some(signal))

            case None => throw TrunkException(Cell.ErrorCodes.SignalBridgeTapless)
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

        // Add peer reports:
        if (sections.HasPeerReports)
        {
            report ++=
                Json.obj(TrunkModel.Glossary.kRSignalTaps -> _trunkModel.ReportSignalTaps(
                    GetTrunkKey,
                    SignalTap.Query(Vector(_refs._tapKey), sectionsOpt)))
        }

        report
    }

    def PropagateTest (signal: Signal[_ >: SignalTypes]): Unit =
    {
        _trunkModel.GetSignalTapOpt(GetTrunkKey, _refs._tapKey) match
        {
            case Some(tap) => tap.Propagate(Some(signal))

            case None => throw TrunkException(Cell.ErrorCodes.SignalInputTapless)
        }
    }

    override
    def Activate (): Unit =
    {
        _trunkModel.GetSignalTapOpt(GetTrunkKey, _refs._tapKey) match
        {
            case Some(tap) => tap.Propagate()

            case None => throw TrunkException(Cell.ErrorCodes.SignalInputTapless)
        }
    }
}
