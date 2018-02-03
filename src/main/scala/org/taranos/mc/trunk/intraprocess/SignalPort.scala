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
import org.taranos.mc.trunk.intraprocess.AliasedElement.AliasedConstructorMetaDecoder
import org.taranos.mc.trunk.intraprocess.BiasedElement.BiasedConstructorMetaDecoder
import org.taranos.mc.trunk.intraprocess.TestableElement.TestableUpdateStateDecoder
import org.taranos.mc.trunk.intraprocess.TrunkElement.{CommonConstructorMetaDecoder, CommonDestructorMetaDecoder,
    CommonQueryDecoder, CommonUpdateMetaDecoder}
import play.api.libs.json.{JsError, JsObject, JsSuccess, Json}


object SignalPort
{
    class Key (uniqueKey: TrunkElement.UniqueKey, symbol: Symbol = 'SignalPort)
        extends BiasedElement.Key(uniqueKey, symbol)
            with TappableElementKey

    class Meta (
        uniqueKey: TrunkElement.UniqueKey,
        tag: String,
        badgeOpt: Option[String],
        nameOpt: Option[String],
        descriptionOpt: Option[String],
        var _aliasOpt: Option[String],
        mode: Signal.ModeEnum.Mode)
        extends BiasedElement.Meta[SignalPort.Key](
            new SignalPort.Key(uniqueKey),
            tag,
            badgeOpt,
            nameOpt,
            descriptionOpt,
            mode)
    {
        override
        def Report (sections: ReportSectionsParser): JsObject =
        {
            var report = super.Report(sections)

            if (_aliasOpt.isDefined)
                report ++=
                    Json.obj(TrunkModel.Glossary.kPMetaAlias -> _aliasOpt.get)

            report
        }
    }

    class Attrs
        extends TrunkElement.Attrs

    class Refs (
        trunkKey: Trunk.Key,
        val _interfaceKey: SignalInterface.Key,
        val _inputKeyOpt: Option[SignalInput.Key])
        extends TrunkElement.Refs(trunkKey)
    {
        override
        def Report (sections: ReportSectionsParser): JsObject =
        {
            var report = super.Report(sections)

            report ++=
                Json.obj(TrunkModel.Glossary.kESignalInterface -> TrunkElement.EncodeKey(_interfaceKey))

            if (_inputKeyOpt.isDefined)
                report ++=
                    Json.obj(TrunkModel.Glossary.kESignalInput -> TrunkElement.EncodeKey(_inputKeyOpt.get))

            report
        }
    }

    class State
        extends TrunkElement.State

    case class Constructor (
        _tag: String,
        _badgeOpt: Option[String] = None,
        _nameOpt: Option[String] = None,
        _descriptionOpt: Option[String] = None,
        _aliasOpt: Option[String],
        _mode: Signal.ModeEnum.Mode,
        _inputKeyOpt: Option[SignalInput.Key])

    case class Destructor (
        _key: SignalPort.Key)

    case class Query (
        keys: Vector[SignalPort.Key],
        sectionsOpt: Option[String] = None)
        extends TrunkElement.Query[SignalPort.Key](
            keys,
            sectionsOpt)

    case class Update (
        _key: SignalPort.Key,
        _nameOpt: Option[String],
        _descriptionOpt: Option[String],
        _aliasOpt: Option[String],
        _signalEncodedOpt: Option[String])

    val kAnyKey = new Key(TrunkModel.Glossary.kAnyKeyBase)

    def DecodeConstructor (encoded: String): Constructor =
    {
        val constructor = Json.parse(encoded)

        val commonMeta = new CommonConstructorMetaDecoder(constructor, Cell.ErrorCodes.SignalPortConstructorInvalid)

        val biasedMeta = new BiasedConstructorMetaDecoder(constructor)
        val mode = biasedMeta._modeOpt match
        {
            case Some(value) =>
                value match
                {
                    case Signal.ModeEnum.Continuous | Signal.ModeEnum.Discrete => value

                    case _ => throw TrunkException(Cell.ErrorCodes.SignalModeInvalid)
                }

            case _ =>
                throw TrunkException(Cell.ErrorCodes.SignalPortConstructorInvalid, "missing mode element")
        }

        val aliasedMeta = new AliasedConstructorMetaDecoder(constructor)

        val inputKeyOpt: Option[SignalInput.Key] =
            (constructor \ TrunkModel.Glossary.kPSRefs \ TrunkModel.Glossary.kESignalInput).validate[String] match
            {
                case JsSuccess(value, _) => Some(TrunkElement.DecodeKey[SignalInput.Key](value))

                case JsError(_) => None
            }

        Constructor(
            commonMeta._tag,
            commonMeta._badgeOpt,
            commonMeta._nameOpt,
            commonMeta._descriptionOpt,
            aliasedMeta._aliasOpt,
            mode,
            inputKeyOpt)
    }

    def DecodeDestructor (encoded: String): Destructor =
    {
        val destructor = Json.parse(encoded)

        val commonMeta = new CommonDestructorMetaDecoder[SignalPort.Key](
            destructor, Cell.ErrorCodes.SignalPortDestructorInvalid)

        Destructor(commonMeta._key)
    }

    def DecodeQuery (encoded: String): Query =
    {
        val query = Json.parse(encoded)

        val commonQuery = new CommonQueryDecoder[SignalPort.Key](query)

        Query(commonQuery._keysOpt.get, commonQuery._sectionsOpt)
    }

    def DecodeUpdate (encoded: String): Update =
    {
        val update = Json.parse(encoded)

        val commonMeta = new CommonUpdateMetaDecoder[SignalPort.Key](
            update, Cell.ErrorCodes.SignalPortUpdateInvalid)

        val aliasedMeta = new AliasedConstructorMetaDecoder(update)

        val testableState = new TestableUpdateStateDecoder(update)

        Update(
            commonMeta._key,
            commonMeta._nameOpt,
            commonMeta._descriptionOpt,
            aliasedMeta._aliasOpt,
            testableState._signalEncodedOpt)
    }
}

class SignalPort (
    meta: SignalPort.Meta,
    attrs: SignalPort.Attrs,
    refs: SignalPort.Refs)
    (implicit protected val _trunkModel: TrunkModel)
    extends BiasedElement[SignalPort.Key]
        with AliasedElement
{
    //
    // Meta:
    //
    protected
    val _meta = meta

    def GetAliasOpt: Option[String]
        = _meta._aliasOpt

    def GetMode: Signal.ModeEnum.Mode
        = _meta._mode

    def SetAliasOpt (aliasOpt: Option[String]): Unit
        = _meta._aliasOpt = aliasOpt

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

    def GetInterfaceKey: SignalInterface.Key =
        _refs._interfaceKey

    def GetInputKeyOpt: Option[SignalInput.Key] =
        _refs._inputKeyOpt

    //
    // State:
    //
    protected
    val _state = null

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

        report
    }
}
