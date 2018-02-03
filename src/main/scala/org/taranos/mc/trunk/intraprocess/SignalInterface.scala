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
import org.taranos.mc.trunk.intraprocess.TrunkElement.{CommonConstructorMetaDecoder, CommonDestructorMetaDecoder, CommonQueryDecoder, CommonUpdateMetaDecoder}
import play.api.libs.json.{JsObject, Json}


object SignalInterface
{
    import scala.collection.mutable

    class Key (uniqueKey: TrunkElement.UniqueKey, symbol: Symbol = 'SignalInterface)
        extends TrunkElement.Key(uniqueKey, symbol)

    class Meta (
        uniqueKey: TrunkElement.UniqueKey,
        tag: String,
        badgeOpt: Option[String] = None,
        nameOpt: Option[String] = None,
        descriptionOpt: Option[String] = None)
        extends TrunkElement.Meta[SignalInterface.Key](
            new SignalInterface.Key(uniqueKey),
            tag,
            badgeOpt,
            nameOpt,
            descriptionOpt)

    class Attrs
        extends TrunkElement.Attrs

    class Refs (
        trunkKey: Trunk.Key,
        val _portKeys: mutable.Set[SignalPort.Key] = mutable.Set.empty[SignalPort.Key])
        extends TrunkElement.Refs(trunkKey)
    {
        override
        def Report (sections: ReportSectionsParser): JsObject =
        {
            var report = super.Report(sections)

            if (!sections.HasChildReports)
                report ++=
                    Json.obj(TrunkModel.Glossary.kESignalPort -> _portKeys.map(TrunkElement.EncodeKey))

            report
        }
    }

    class State
        extends TrunkElement.State

    case class Constructor (
        _tag: String,
        _badgeOpt: Option[String] = None,
        _nameOpt: Option[String] = None,
        _descriptionOpt: Option[String] = None)

    case class Destructor (
        _key: SignalInterface.Key)

    case class Query (
        keys: Vector[SignalInterface.Key],
        sectionsOpt: Option[String] = None)
        extends TrunkElement.Query[SignalInterface.Key](
            keys,
            sectionsOpt)

    case class Update (
        _key: SignalInterface.Key,
        _nameOpt: Option[String],
        _descriptionOpt: Option[String])

    val kAnyKey = new Key(TrunkModel.Glossary.kAnyKeyBase)

    val kNoneKey = new Key(TrunkModel.Glossary.kNoneKeyBase)

    def DecodeConstructor (encoded: String): Constructor =
    {
        val constructor = Json.parse(encoded)

        val commonMeta = new CommonConstructorMetaDecoder(constructor, Cell.ErrorCodes.SignalLinkConstructorInvalid)

        Constructor(
            commonMeta._tag,
            commonMeta._badgeOpt,
            commonMeta._nameOpt,
            commonMeta._descriptionOpt)
    }

    def DecodeDestructor (encoded: String): Destructor =
    {
        val destructor = Json.parse(encoded)

        val commonMeta = new CommonDestructorMetaDecoder[SignalInterface.Key](
            destructor, Cell.ErrorCodes.SignalInterfaceDestructorInvalid)

        Destructor(commonMeta._key)
    }

    def DecodeQuery (encoded: String): Query =
    {
        val query = Json.parse(encoded)

        val commonQuery = new CommonQueryDecoder[SignalInterface.Key](query)

        Query(commonQuery._keysOpt.get, commonQuery._sectionsOpt)
    }

    def DecodeUpdate (encoded: String): Update =
    {
        val update = Json.parse(encoded)

        val commonMeta = new CommonUpdateMetaDecoder[SignalInterface.Key](
            update, Cell.ErrorCodes.SignalInterfaceUpdateInvalid)

        Update(
            commonMeta._key,
            commonMeta._nameOpt,
            commonMeta._descriptionOpt)
    }
}

class SignalInterface (
    meta: SignalInterface.Meta,
    attrs: SignalInterface.Attrs,
    refs: SignalInterface.Refs)
    (implicit protected val _trunkModel: TrunkModel)
    extends TrunkElement[SignalInterface.Key]
{
    import scala.collection.mutable

    //
    // Meta:
    //
    protected
    val _meta = meta

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

    def BindPort (key: SignalPort.Key): Unit =
        _refs._portKeys += key

    def GetPortKeys: mutable.Set[SignalPort.Key] =
        _refs._portKeys

    def UnbindPort (key: SignalPort.Key): Unit =
        _refs._portKeys -= key

    //
    // State:
    //
    protected
    val _state = null

    //
    // Reporting:
    //
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
            if (_refs._portKeys.nonEmpty)
                report ++=
                    Json.obj(TrunkModel.Glossary.kRSignalPorts -> _trunkModel.ReportSignalPorts(
                        GetTrunkKey,
                        GetKey,
                        SignalPort.Query(_refs._portKeys.toVector, sectionsOpt)))
        }

        report
    }
}
