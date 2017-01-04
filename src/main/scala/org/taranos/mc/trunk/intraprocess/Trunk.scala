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
import org.taranos.mc.trunk.intraprocess.TrunkElement.{CommonConstructorMetaDecoder, CommonDestructorMetaDecoder, CommonQueryDecoder, CommonUpdateMetaDecoder}
import play.api.libs.json.{JsObject, Json}


object Trunk
{
    import scala.collection.mutable

    class Key (uniqueKey: TrunkElement.UniqueKey, symbol: Symbol = 'Trunk)
        extends TrunkElement.Key(uniqueKey, symbol)

    class Meta (
        uniqueKey: TrunkElement.UniqueKey,
        tag: String,
        badgeOpt: Option[String] = None,
        nameOpt: Option[String] = None,
        descriptionOpt: Option[String] = None)
        extends TrunkElement.Meta[Trunk.Key](
            new Trunk.Key(uniqueKey),
            tag,
            badgeOpt,
            nameOpt,
            descriptionOpt)

    class Attrs
        extends TrunkElement.Attrs

    class Refs (
        val _interfaceKeys: mutable.Set[SignalInterface.Key] = mutable.Set.empty[SignalInterface.Key],
        val _portKeys: mutable.Set[SignalPort.Key] = mutable.Set.empty[SignalPort.Key],
        val _sourceKeys: mutable.Set[SignalSource.Key] = mutable.Set.empty[SignalSource.Key],
        val _sinkKeys: mutable.Set[SignalSink.Key] = mutable.Set.empty[SignalSink.Key],
        val _linkKeys: mutable.Set[SignalLink.Key] = mutable.Set.empty[SignalLink.Key],
        val _tapKeys: mutable.Set[SignalTap.Key] = mutable.Set.empty[SignalTap.Key],
        val _inputKeys: mutable.Set[SignalInput.Key] = mutable.Set.empty[SignalInput.Key],
        val _bridgeKeys: mutable.Set[SignalBridge.Key] = mutable.Set.empty[SignalBridge.Key],
        val _outputKeys: mutable.Set[SignalOutput.Key] = mutable.Set.empty[SignalOutput.Key],
        val _emitterPatchKeys: mutable.Set[EmitterPatch.Key] = mutable.Set.empty[EmitterPatch.Key],
        val _oscillatorPatchKeys: mutable.Set[OscillatorPatch.Key] = mutable.Set.empty[OscillatorPatch.Key],

        var _defaultInterfaceKey: SignalInterface.Key = SignalInterface.kNoneKey)
        extends TrunkElement.Refs(Trunk.kNoneKey)
    {
        override
        def Report (sections: ReportSectionsParser): JsObject =
        {
            var report = super.Report(sections)

            if (!sections.HasChildReports)
            {
                // Add interface refs:
                if (_interfaceKeys.nonEmpty)
                    report ++=
                        Json.obj(TrunkModel.Glossary.kESignalInterface -> _interfaceKeys.map(TrunkElement.EncodeKey))

                // Add port refs:
                if (_portKeys.nonEmpty)
                    report ++=
                        Json.obj(TrunkModel.Glossary.kESignalPort -> _portKeys.map(TrunkElement.EncodeKey))

                // Add source refs:
                if (_sourceKeys.nonEmpty)
                    report ++=
                        Json.obj(TrunkModel.Glossary.kESignalSource -> _sourceKeys.map(TrunkElement.EncodeKey))

                // Add sink refs:
                if (_sinkKeys.nonEmpty)
                    report ++=
                        Json.obj(TrunkModel.Glossary.kESignalSink -> _sinkKeys.map(TrunkElement.EncodeKey))

                // Add link refs:
                if (_linkKeys.nonEmpty)
                    report ++=
                        Json.obj(TrunkModel.Glossary.kESignalLink -> _linkKeys.map(TrunkElement.EncodeKey))

                // Add tap refs:
                if (_tapKeys.nonEmpty)
                    report ++=
                        Json.obj(TrunkModel.Glossary.kESignalTap -> _tapKeys.map(TrunkElement.EncodeKey))

                // Add input refs:
                if (_inputKeys.nonEmpty)
                    report ++=
                        Json.obj(TrunkModel.Glossary.kESignalInput -> _inputKeys.map(TrunkElement.EncodeKey))

                // Add bridge refs:
                if (_bridgeKeys.nonEmpty)
                    report ++=
                        Json.obj(TrunkModel.Glossary.kESignalBridge -> _bridgeKeys.map(TrunkElement.EncodeKey))

                // Add output refs:
                if (_outputKeys.nonEmpty)
                    report ++=
                        Json.obj(TrunkModel.Glossary.kESignalOutput -> _outputKeys.map(TrunkElement.EncodeKey))

                // Add emitter patch refs:
                if (_emitterPatchKeys.nonEmpty)
                    report ++=
                        Json.obj(TrunkModel.Glossary.kEEmitterPatch -> _emitterPatchKeys.map(TrunkElement.EncodeKey))

                // Add oscillator patch refs:
                if (_oscillatorPatchKeys.nonEmpty)
                    report ++= Json.obj(TrunkModel.Glossary.kEOscillatorPatch ->
                        _oscillatorPatchKeys.map(TrunkElement.EncodeKey))
            }

            report ++=
                Json.obj(TrunkModel.Glossary.kEDefaultSignalInterface -> TrunkElement.EncodeKey(_defaultInterfaceKey))

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
        _key: Trunk.Key)

    case class Query (
        keys: Vector[Trunk.Key],
        sectionsOpt: Option[String] = None)
        extends TrunkElement.Query[Trunk.Key](
            keys,
            sectionsOpt)

    case class Update (
        _key: Trunk.Key,
        _nameOpt: Option[String],
        _descriptionOpt: Option[String])

    val kAnyKey = new Key(TrunkModel.Glossary.kAnyKeyBase)

    val kNoneKey = new Key(TrunkModel.Glossary.kNoneKeyBase)

    def DecodeConstructor (encoded: String): Constructor =
    {
        val constructor = Json.parse(encoded)

        val commonMeta = new CommonConstructorMetaDecoder(constructor, Cell.ErrorCodes.TrunkConstructorInvalid)

        new Constructor(
            commonMeta._tag,
            commonMeta._badgeOpt,
            commonMeta._nameOpt,
            commonMeta._descriptionOpt)
    }

    def DecodeDestructor (encoded: String): Destructor =
    {
        val destructor = Json.parse(encoded)

        val commonMeta = new CommonDestructorMetaDecoder[Trunk.Key](
            destructor, Cell.ErrorCodes.TrunkDestructorInvalid)

        new Destructor(commonMeta._key)
    }

    def DecodeQuery (encoded: String): Query =
    {
        val query = Json.parse(encoded)

        val commonQuery = new CommonQueryDecoder[Trunk.Key](query)

        new Query(commonQuery._keysOpt.get, commonQuery._sectionsOpt)
    }

    def DecodeUpdate (encoded: String): Update =
    {
        val update = Json.parse(encoded)

        val commonMeta = new CommonUpdateMetaDecoder[Trunk.Key](
            update, Cell.ErrorCodes.TrunkUpdateInvalid)

        new Update(
            commonMeta._key,
            commonMeta._nameOpt,
            commonMeta._descriptionOpt)
    }
}

class Trunk (
    meta: Trunk.Meta,
    attrs: Trunk.Attrs,
    refs: Trunk.Refs)
    (implicit protected val _trunkModel: TrunkModel)
    extends TrunkElement[Trunk.Key]
{
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

    def BindSignalInterface (key: SignalInterface.Key) =
    {
        // First interface becomes the default interface:
        if (_refs._interfaceKeys.isEmpty)
            _refs._defaultInterfaceKey = key
        _refs._interfaceKeys += key
    }

    def GetDefaultSignalInterfaceKey = _refs._defaultInterfaceKey

    def GetSignalInterfaceKeys = _refs._interfaceKeys.toSet

    def UnbindSignalInterface (key: SignalInterface.Key) = _refs._interfaceKeys -= key

    def BindSignalPort (key: SignalPort.Key) = _refs._portKeys += key

    def GetSignalPortKeys = _refs._portKeys.toSet

    def UnbindSignalPort (key: SignalPort.Key) = _refs._portKeys -= key

    def BindSignalSource (key: SignalSource.Key) = _refs._sourceKeys += key

    def GetSignalSourceKeys = _refs._sourceKeys.toSet

    def UnbindSignalSource (key: SignalSource.Key) = _refs._sourceKeys -= key

    def BindSignalSink (key: SignalSink.Key) = _refs._sinkKeys += key

    def GetSignalSinkKeys = _refs._sinkKeys.toSet

    def UnbindSignalSink (key: SignalSink.Key) = _refs._sinkKeys -= key

    def BindSignalLink (key: SignalLink.Key) = _refs._linkKeys += key

    def GetSignalLinkKeys = _refs._linkKeys.toSet

    def UnbindSignalLink (key: SignalLink.Key) = _refs._linkKeys -= key

    def BindSignalTap (key: SignalTap.Key) = _refs._tapKeys += key

    def GetSignalTapKeys = _refs._tapKeys.toSet

    def UnbindSignalTap (key: SignalTap.Key) = _refs._tapKeys -= key

    def BindSignalInput (key: SignalInput.Key) = _refs._inputKeys += key

    def GetSignalInputKeys = _refs._inputKeys.toSet

    def UnbindSignalInput (key: SignalInput.Key) = _refs._inputKeys -= key

    def BindSignalBridge (key: SignalBridge.Key) = _refs._bridgeKeys += key

    def GetSignalBridgeKeys = _refs._bridgeKeys.toSet

    def UnbindSignalBridge (key: SignalBridge.Key) = _refs._bridgeKeys -= key

    def BindSignalOutput (key: SignalOutput.Key) = _refs._outputKeys += key

    def GetSignalOutputKeys = _refs._outputKeys.toSet

    def UnbindSignalOutput (key: SignalOutput.Key) = _refs._outputKeys -= key

    def BindEmitterPatch (key: EmitterPatch.Key) = _refs._emitterPatchKeys += key

    def GetEmitterPatchKeys = _refs._emitterPatchKeys.toSet

    def UnbindEmitterPatch (key: EmitterPatch.Key) = _refs._emitterPatchKeys -= key

    def BindOscillatorPatch (key: OscillatorPatch.Key) = _refs._oscillatorPatchKeys += key

    def GetOscillatorPatchKeys = _refs._oscillatorPatchKeys.toSet

    def UnbindOscillatorPatch (key: OscillatorPatch.Key) = _refs._oscillatorPatchKeys -= key

    //
    // State:
    //
    protected
    val _state = null

    def GetMarked (mark: Int): Vector[TrunkElement.Key] =
    {
        val keys =
            _refs._sourceKeys ++
            _refs._sinkKeys ++
            _refs._linkKeys ++
            _refs._tapKeys

        val markedKeys =
            for
            {
                key <- keys
                if (key match
                {
                    case k: SignalSource.Key =>
                        val source = _trunkModel.GetSignalSourceOpt(GetKey, k).get
                        source.GetMark == mark

                    case k: SignalSink.Key =>
                        val sink = _trunkModel.GetSignalSinkOpt(GetKey, k).get
                        sink.GetMark == mark

                    case k: SignalLink.Key =>
                        val link = _trunkModel.GetSignalLinkOpt(GetKey, k).get
                        link.GetMark == mark

                    case k: SignalTap.Key =>
                        val tap = _trunkModel.GetSignalTapOpt(GetKey, k).get
                        tap.GetMark == mark
                })
            } yield key
        markedKeys.toVector
    }

    def GetTrunkModel = _trunkModel

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
            if (_refs._interfaceKeys.nonEmpty)
                report ++=
                    Json.obj(TrunkModel.Glossary.kRSignalInterfaces -> _trunkModel.ReportSignalInterfaces(
                        GetKey,
                        new SignalInterface.Query(_refs._interfaceKeys.toVector, sectionsOpt)))

            if (_refs._portKeys.nonEmpty)
                report ++=
                    Json.obj(TrunkModel.Glossary.kRSignalPorts -> _trunkModel.ReportSignalPorts(
                        GetKey,
                        SignalInterface.kAnyKey,
                        new SignalPort.Query(_refs._portKeys.toVector, sectionsOpt)))

            if (_refs._sourceKeys.nonEmpty)
                report ++=
                    Json.obj(TrunkModel.Glossary.kRSignalSources -> _trunkModel.ReportSignalSources(
                        GetKey,
                        new SignalSource.Query(_refs._sourceKeys.toVector, sectionsOpt)))

            if (_refs._sinkKeys.nonEmpty)
                report ++=
                    Json.obj(TrunkModel.Glossary.kRSignalSinks -> _trunkModel.ReportSignalSinks(
                        GetKey,
                        new SignalSink.Query(_refs._sinkKeys.toVector, sectionsOpt)))

            if (_refs._linkKeys.nonEmpty)
                report ++=
                    Json.obj(TrunkModel.Glossary.kRSignalLinks -> _trunkModel.ReportSignalLinks(
                        GetKey,
                        new SignalLink.Query(_refs._linkKeys.toVector, sectionsOpt)))

            if (_refs._tapKeys.nonEmpty)
                report ++=
                    Json.obj(TrunkModel.Glossary.kRSignalTaps -> _trunkModel.ReportSignalTaps(
                        GetKey,
                        new SignalTap.Query(_refs._tapKeys.toVector, sectionsOpt)))

            if (_refs._inputKeys.nonEmpty)
                report ++=
                    Json.obj(TrunkModel.Glossary.kRSignalInputs -> _trunkModel.ReportSignalInputs(
                        GetKey,
                        new SignalInput.Query(_refs._inputKeys.toVector, sectionsOpt)))

            if (_refs._bridgeKeys.nonEmpty)
                report ++=
                    Json.obj(TrunkModel.Glossary.kRSignalBridges -> _trunkModel.ReportSignalBridges(
                        GetKey,
                        new SignalBridge.Query(_refs._bridgeKeys.toVector, sectionsOpt)))

            if (_refs._outputKeys.nonEmpty)
                report ++=
                    Json.obj(TrunkModel.Glossary.kRSignalOutputs -> _trunkModel.ReportSignalOutputs(
                        GetKey,
                        new SignalOutput.Query(_refs._outputKeys.toVector, sectionsOpt)))

            if (_refs._emitterPatchKeys.nonEmpty)
                report ++=
                    Json.obj(TrunkModel.Glossary.kREmitterPatches -> _trunkModel.ReportEmitterPatches(
                        GetKey,
                        new EmitterPatch.Query(_refs._emitterPatchKeys.toVector, sectionsOpt)))

            if (_refs._oscillatorPatchKeys.nonEmpty)
                report ++=
                    Json.obj(TrunkModel.Glossary.kROscillatorPatches -> _trunkModel.ReportOscillatorPatches(
                        GetKey,
                        EmitterPatch.kAnyKey,
                        new OscillatorPatch.Query(_refs._oscillatorPatchKeys.toVector, sectionsOpt)))
        }

        report
    }
}
