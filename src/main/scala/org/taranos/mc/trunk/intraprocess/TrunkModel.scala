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

import org.taranos.mc.Common.{ReportSectionsParser, Reporter}
import org.taranos.mc.field._
import org.taranos.mc.trunk.intraprocess.TrunkElement.CommonQueryDecoder
import org.taranos.mc.trunk.intraprocess.TrunkModel.TrunkModelReporter
import org.taranos.mc.{Cell, CellLogger}
import play.api.libs.json.{JsObject, Json}


/*

Marking strategy
----------------
source      self
sink        self if all links marked; all links pre-marked if directly destroyed
link        self
tap         self; mark path at sink if directly destroyed

Propagation only continues if mark accepted.

*/


object TrunkModel
{
    case class Query (
        _sectionsOpt: Option[String] = None)

    def DecodeQuery (encoded: String): Query =
    {
        val query = Json.parse(encoded)

        val commonQuery = new CommonQueryDecoder[TrunkElement.Key](query, isKeysRequired = false)

        Query(commonQuery._sectionsOpt)
    }

    trait TrunkModelReporter
        extends Reporter
    {
        def Report (sectionsOpt: Option[String]): JsObject =
            Json.obj()

        protected[mc]
        def Report (trunkKey: Trunk.Key, sectionsOpt: Option[String]): JsObject
    }

    // String glossary:
    object Glossary
    {
        // punctuation
        val kDefaultPrefix = "_"
        val kBang = "!"
        val kTagSeparator = "."
        val kTagSubSeparator = "-"
        val kPartSeparator = "~"

        // elements
        val kETrunk = "t"
        val kESignal = "s"
        val kESignalInterface = "si"
        val kESignalPort = "sp"
        val kESignalSource = "ss"
        val kESignalSink = "sk"
        val kESignalLink = "sl"
        val kESignalTap = "st"
        val kESignalModulator = "sm"
        val kESignalInput = kESignalModulator + "i"
        val kESignalBridge = kESignalModulator + "b"
        val kESignalOutput = kESignalModulator + "o"
        val kESignalModulatorPatch = kESignalModulator + "p"
        val kEEmitterPatch = kESignalModulatorPatch + "e"
        val kEOscillatorPatch = kESignalModulatorPatch + "o"
        val kEDefaultTrunk = kDefaultPrefix + kETrunk
        val kEDefaultSignalInterface = kDefaultPrefix + kESignalInterface

        // subelements
        val kPart = "p"
        val kTrap = "t"
        
        // property sets and properties
        val kPSMeta = "m"
        val kPMetaKey = "k"
        val kPMetaTag = "t"
        val kPMetaBadge = "b"
        val kPMetaName = "n"
        val kPMetaDescription = "d"
        val kPMetaAlias = "a"
        val kPSAttrs = "a"
        val kPSRefs = "r"
        val kPSState = "s"
        val kPSChildren = "c"
        val kPSPeers = "p"
        val kPQuerySections = "s"

        // reports
        val kReports = "r"
        val kRTrunks = kReports + kETrunk
        val kRSignalInterfaces = kReports + kESignalInterface
        val kRSignalPorts = kReports + kESignalPort
        val kRSignalSources = kReports + kESignalSource
        val kRSignalSinks = kReports + kESignalSink
        val kRSignalLinks = kReports + kESignalLink
        val kRSignalTaps = kReports + kESignalTap
        val kRSignalInputs = kReports + kESignalInput
        val kRSignalBridges = kReports + kESignalBridge
        val kRSignalOutputs = kReports + kESignalOutput
        val kREmitterPatches = kReports + kEEmitterPatch
        val kROscillatorPatches = kReports + kEOscillatorPatch
        val kROscillatorPatchEnvelopes = kReports + "e" + kEOscillatorPatch
        val kRStandardCreationSections = kPSAttrs + kPSRefs + kPSState + kPSChildren
        val kRModulatorCreationSections = kPSAttrs + kPSRefs + kPSState + kPSPeers

        val kRElementCount = "ec"

        // lookups
        val kLookups = "l"
        val kLSignalPorts = kLookups + kESignalPort

        // miscellaneous
        val kAnyKeyBase = "~"
        val kNoneKeyBase = ""
        val kSignalMode = "m"
        val kSignalOrdinal = "o"
        val kSignalValue = "v"
        val kSignalModeContinuous = "c"
        val kSignalModeDiscrete = "d"
        val kSignalModeUnbiased = "u"
    }
}

class TrunkModel (
    val _cell: Cell)
    (implicit val _logger: CellLogger)
    extends TrunkModelReporter
{
    implicit
    val _trunkModel = this

    private
    var _fieldModelOpt: Option[FieldModel] = None

    def BindFieldModel (fieldModel: FieldModel): Unit =
        _fieldModelOpt = Some(fieldModel)

    def GetFieldModel: FieldModel =
        _fieldModelOpt.getOrElse(throw TrunkException(Cell.ErrorCodes.TrunkInvalid))

    private
    var _defaultTrunkOpt: Option[Trunk] = None

    private
    val _trunkPlant = new TrunkPlant

    private
    val _signalInterfacePlant = new SignalInterfacePlant

    private
    val _signalPortPlant = new SignalPortPlant

    private
    val _signalSourcePlant = new SignalSourcePlant

    private
    val _signalSinkPlant = new SignalSinkPlant

    private
    val _signalLinkPlant = new SignalLinkPlant

    private
    val _signalTapPlant = new SignalTapPlant

    private
    val _signalInputPlant = new SignalInputPlant

    private
    val _signalBridgePlant = new SignalBridgePlant

    private
    val _signalOutputPlant = new SignalOutputPlant

    private
    val _emitterPatchPlant = new EmitterPatchPlant

    private
    val _oscillatorPatchPlant = new OscillatorPatchPlant

    protected
    var _signalOrdinal = 0

    def CreateSignal[Mode >: Signal.SignalTypes] (scalar: Mode): Signal[Mode] =
    {
        val signalOrdinal = scalar match
        {
            case _: Signal.Virtual => _cell.GetCycleOrdinal

            case _ =>
                _signalOrdinal = _signalOrdinal + 1
                _signalOrdinal
        }
        new Signal[Mode](signalOrdinal, scalar)
    }

    def Kill (killOrder: Cell.KillOrder): Unit =
    {
        // Continue kill only if kill order's trunk is still valid:
        val trunkOpt: Option[Trunk] =
            try
                Some(GetTrunk(killOrder._trunkKey))
            catch
            {
                case exc: TrunkException =>
                    if (exc._code != Cell.ErrorCodes.TrunkUnknown) throw exc
                    None

                case exc: Throwable => throw exc
            }
        trunkOpt.foreach(trunk =>
        {
//val markedKeys = trunk.GetMarked(killOrder._mark)

            trunk.GetSignalSourceKeys.foreach(key =>
            {
                val source = GetSignalSourceOpt(trunk.GetKey, key).get
                if (source.GetMark == killOrder._mark)
                {
                    val destructor = SignalSource.Destructor(key)
                    _signalSourcePlant.DestroySignalSource(trunk, destructor)
                }
            })

            trunk.GetSignalSinkKeys.foreach(key =>
            {
                val sink = GetSignalSinkOpt(trunk.GetKey, key).get
                if (sink.GetMark == killOrder._mark)
                {
                    val destructor = SignalSink.Destructor(key)
                    _signalSinkPlant.DestroySignalSink(trunk, destructor)
                }
            })

            trunk.GetSignalLinkKeys.foreach(key =>
            {
                val link = GetSignalLinkOpt(trunk.GetKey, key).get
                if (link.GetMark == killOrder._mark)
                {
                    val destructor = SignalLink.Destructor(key)
                    _signalLinkPlant.DestroySignalLink(trunk, destructor)
                }
            })

            trunk.GetSignalTapKeys.foreach(key =>
            {
                val tap = GetSignalTapOpt(trunk.GetKey, key).get
                if (tap.GetMark == killOrder._mark)
                {
                    val destructor = SignalTap.Destructor(key)
                    _signalTapPlant.DestroySignalTap(trunk, destructor)
                }
            })
        })
    }

    private
    def CommonCreater[ConstructorType, ElementType <: TrunkElement[_ <: TrunkElement.Key]] (
        trunkKey: Trunk.Key,
        constructors: Vector[ConstructorType],
        createrFunc: (Trunk, ConstructorType) => ElementType): Vector[ElementType] =
    {
        // Get trunk (validate):
        val trunk = GetTrunk(trunkKey)

        // Iterate constructors creating elements:
        constructors.map(createrFunc(trunk, _))
    }

    private
    def CommonReporter[KeyType <: TrunkElement.Key] (
        trunkKey: Trunk.Key,
        query: TrunkElement.Query[KeyType],
        getKeysFunc: => Set[KeyType],
        getReporterFunc: (Trunk.Key, KeyType, Boolean) => Option[Reporter]): Vector[JsObject] =
    {
        // Get trunk (validate):
        GetTrunk(trunkKey)

        // Calculate valid keys as intersect of requested keys and keys known by trunk:
        val knownKeys = getKeysFunc
        val validKeys = if (query._keys.isEmpty) knownKeys else knownKeys.intersect(query.GetKeys.toSet)

        // Iterate valid keys:
        val reports = validKeys.map(key =>
        {
            val reporter = getReporterFunc(trunkKey, key, true).get
            reporter.Report(query._sectionsOpt)
        })

        // Return reports vector:
        reports.toVector
    }

    def Report (trunkKey: Trunk.Key = Trunk.kAnyKey, sectionsOpt: Option[String] = None): JsObject =
    {
        var report = Json.obj()

        val sections = new ReportSectionsParser(sectionsOpt)

        if (_defaultTrunkOpt.isDefined)
            report ++=
                Json.obj(TrunkModel.Glossary.kEDefaultTrunk -> TrunkElement.EncodeKey(_defaultTrunkOpt.get.GetKey))

        // Add child reports:
        if (sections.HasChildReports)
        {
            // Add plant reports:
            var plantReports = Json.obj()
            val trunkKeys: Vector[Trunk.Key] =
                if (trunkKey == Trunk.kAnyKey)
                    _trunkPlant.GetTrunkKeys
                else
                    Vector(trunkKey)
            trunkKeys.foreach(trunkKey =>
            {
                plantReports = plantReports ++ Json.obj(
                    TrunkElement.EncodeKey(trunkKey) -> Json.obj(
                        TrunkModel.Glossary.kETrunk -> _trunkPlant.Report(trunkKey),
                        TrunkModel.Glossary.kESignalInterface -> _signalInterfacePlant.Report(trunkKey),
                        TrunkModel.Glossary.kESignalPort -> _signalPortPlant.Report(trunkKey),
                        TrunkModel.Glossary.kESignalSource -> _signalSourcePlant.Report(trunkKey),
                        TrunkModel.Glossary.kESignalSink -> _signalSinkPlant.Report(trunkKey),
                        TrunkModel.Glossary.kESignalLink -> _signalLinkPlant.Report(trunkKey),
                        TrunkModel.Glossary.kESignalTap -> _signalTapPlant.Report(trunkKey),
                        TrunkModel.Glossary.kESignalInput -> _signalInputPlant.Report(trunkKey),
                        TrunkModel.Glossary.kESignalBridge -> _signalBridgePlant.Report(trunkKey),
                        TrunkModel.Glossary.kESignalOutput -> _signalOutputPlant.Report(trunkKey),
                        TrunkModel.Glossary.kEEmitterPatch -> _emitterPatchPlant.Report(trunkKey, sectionsOpt),
                        TrunkModel.Glossary.kEOscillatorPatch -> _oscillatorPatchPlant.Report(trunkKey)))
            })
            report ++= Json.obj("rp" -> plantReports)
        }

        report
    }

    //
    // Trunks:
    //

    def CreateTrunks (constructors: Vector[Trunk.Constructor]): Vector[Trunk] =
    {
        // Iterate constructors creating trunks:
        constructors.map(constructor =>
        {
            _trunkPlant.CreateTrunk(constructor)
        })
    }

    def DestroyTrunks (destructors: Vector[Trunk.Destructor]): Unit =
    {
        if (destructors.isEmpty)
            throw TrunkException(Cell.ErrorCodes.TrunkDestructorInvalid)
        else
        {
            // Iterate destructors:
            destructors.foreach(destructor =>
            {
                val trunkKeys: Vector[Trunk.Key] =
                    if (destructor._key == Trunk.kAnyKey)
                    {
                        _trunkPlant.GetTrunkKeys
                    }
                    else
                    {
                        // Get trunk (validate):
                        GetTrunk(destructor._key)

                        Vector(destructor._key)
                    }

                trunkKeys.foreach(trunkKey =>
                {
                    val trunk = GetTrunk(trunkKey)

                    // Since we're destroying the trunk element we can't do propagation-kills, so
                    // we simply brute-force destroy all of the trunks elements at this point:
                    _signalInterfacePlant.DestroyAllSignalInterfaces(trunk)
                    _signalPortPlant.DestroyAllSignalPorts(trunk)
                    _signalSourcePlant.DestroyAllSignalSources(trunk)
                    _signalSinkPlant.DestroyAllSignalSinks(trunk)
                    _signalLinkPlant.DestroyAllSignalLinks(trunk)
                    _signalTapPlant.DestroyAllSignalTaps(trunk)
                    _signalInputPlant.DestroyAllSignalInputs(trunk)
                    _signalBridgePlant.DestroyAllSignalBridges(trunk)
                    _signalOutputPlant.DestroyAllSignalOutputs(trunk)
                    _emitterPatchPlant.DestroyAllEmitterPatches(trunk)
                    _oscillatorPatchPlant.DestroyAllOscillatorPatches(trunk)
                    _trunkPlant.DestroyTrunk(Trunk.Destructor(trunkKey))

                    val elementCount = DumpElementCounts(trunkKey)
                    if (elementCount > 0) assert(false)
                })
            })
        }
    }

    def DumpElementCounts (trunkKey: Trunk.Key): Int =
    {
        val elementCount =
            _trunkPlant.GetElementCount(trunkKey) +
            _signalInterfacePlant.GetElementCount(trunkKey) +
            _signalPortPlant.GetElementCount(trunkKey) +
            _signalSourcePlant.GetElementCount(trunkKey) +
            _signalSinkPlant.GetElementCount(trunkKey) +
            _signalLinkPlant.GetElementCount(trunkKey) +
            _signalTapPlant.GetElementCount(trunkKey) +
            _signalInputPlant.GetElementCount(trunkKey) +
            _signalBridgePlant.GetElementCount(trunkKey) +
            _signalOutputPlant.GetElementCount(trunkKey) +
            _emitterPatchPlant.GetElementCount(trunkKey) +
            _oscillatorPatchPlant.GetElementCount(trunkKey)

        _logger.LogDebug(s"@ $elementCount trunk elements remaining")
        if (elementCount > 0)
        {
            _logger.LogDebug(s"@ t    ${_trunkPlant.GetElementCount(trunkKey)}")
            _logger.LogDebug(s"@ si   ${_signalInterfacePlant.GetElementCount(trunkKey)}")
            _logger.LogDebug(s"@ sp   ${_signalPortPlant.GetElementCount(trunkKey)}")
            _logger.LogDebug(s"@ ss   ${_signalSourcePlant.GetElementCount(trunkKey)}")
            _logger.LogDebug(s"@ sk   ${_signalSinkPlant.GetElementCount(trunkKey)}")
            _logger.LogDebug(s"@ sl   ${_signalLinkPlant.GetElementCount(trunkKey)}")
            _logger.LogDebug(s"@ st   ${_signalTapPlant.GetElementCount(trunkKey)}")
            _logger.LogDebug(s"@ smi  ${_signalInputPlant.GetElementCount(trunkKey)}")
            _logger.LogDebug(s"@ smb  ${_signalBridgePlant.GetElementCount(trunkKey)}")
            _logger.LogDebug(s"@ smo  ${_signalOutputPlant.GetElementCount(trunkKey)}")
            _logger.LogDebug(s"@ smpe ${_emitterPatchPlant.GetElementCount(trunkKey)}")
            _logger.LogDebug(s"@ smpo ${_oscillatorPatchPlant.GetElementCount(trunkKey)}")
        }
        
        elementCount
    }
    
    def GetTrunk (trunkKey: Trunk.Key): Trunk =
    {
        // Return trunk:
        _trunkPlant.GetTrunkOpt(trunkKey).getOrElse{
            assert(true)
            throw TrunkException(Cell.ErrorCodes.TrunkUnknown)}
    }

    def ReportTrunks (query: Trunk.Query): Vector[JsObject] =
    {
        // Calculate valid keys as intersect of requested keys and keys owned by trunk:
        val knownKeys = _trunkPlant.GetTrunkKeys.toSet
        val validKeys = if (query.keys.isEmpty) knownKeys else knownKeys.intersect(query.keys.toSet)

        // Get reports:
        val reports = validKeys.map(key =>
        {
            val trunk = GetTrunk(key)
            trunk.Report(query.sectionsOpt)
        })

        // Return reports vector:
        reports.toVector
    }

    def UpdateTrunks (updates: Vector[Trunk.Update]): Unit =
    {
        // Iterate updates:
        updates.foreach(update =>
        {
            // Get trunk:
            val trunk = GetTrunk(update._key)

            // Update name:
            update._nameOpt.foreach(name => trunk.SetNameOpt(Some(name)))

            // Update description:
            update._descriptionOpt.foreach(description => trunk.SetDescriptionOpt(Some(description)))
        })
    }

    //
    // Signal Interfaces:
    //

    def CreateSignalInterfaces(
        trunkKey: Trunk.Key,
        constructors: Vector[SignalInterface.Constructor]): Vector[SignalInterface] =
    {
        CommonCreater[SignalInterface.Constructor, SignalInterface](
            trunkKey,
            constructors,
            _signalInterfacePlant.CreateSignalInterface)
    }

    def DestroySignalInterfaces(
        trunkKey: Trunk.Key,
        destructors: Vector[SignalInterface.Destructor]): Unit =
    {
        // Get trunk (validate):
        val trunk = GetTrunk(trunkKey)

        if (destructors.isEmpty)
            throw TrunkException(Cell.ErrorCodes.SignalInterfaceDestructorInvalid)
        else
        {
            // Iterate destructors:
            destructors.foreach(destructor =>
            {
                if (destructor._key == SignalInterface.kAnyKey)
                {
                    // Destroy all sources:
                    _signalInterfacePlant.DestroyAllSignalInterfaces(trunk)
                }
                else
                {
                    // Get interface (validate):
                    GetSignalInterfaceOpt (trunk.GetKey, destructor._key)

                    // Destroy interface:
                    _signalInterfacePlant.DestroySignalInterface(trunk, destructor)
                }
            })
        }
    }

    def GetSignalInterfaceOpt (
        trunkKey: Trunk.Key,
        key: SignalInterface.Key,
        isRequired: Boolean = true): Option[SignalInterface] =
    {
        // Get trunk (validate):
        val trunk = GetTrunk(trunkKey)

        // Return interface:
        _signalInterfacePlant.GetSignalInterfaceOpt(
            trunk,
            key,
            isRequired)
    }

    def ReportSignalInterfaces(
        trunkKey: Trunk.Key,
        query: SignalInterface.Query): Vector[JsObject] =
    {
        // Get trunk (validate):
        val trunk = GetTrunk(trunkKey)

        CommonReporter[SignalInterface.Key](
            trunkKey,
            query,
            trunk.GetSignalInterfaceKeys,
            GetSignalInterfaceOpt)
    }

    def UpdateSignalInterfaces(
        trunkKey: Trunk.Key,
        updates: Vector[SignalInterface.Update]): Unit =
    {
        // Get trunk (validate):
        val trunk = GetTrunk(trunkKey)

        // Iterate updates:
        updates.foreach(update =>
        {
            _signalInterfacePlant.GetSignalInterfaceOpt(trunk, update._key) match
            {
                // If interface is known, update it:
                case Some(interface) =>
                    // Update name:
                    update._nameOpt.foreach(name => interface.SetNameOpt(Some(name)))

                    // Update description:
                    update._descriptionOpt.foreach(description => interface.SetDescriptionOpt(Some(description)))

                case None =>
                    throw TrunkException(Cell.ErrorCodes.SignalInterfaceUnknown)
            }
        })
    }

    //
    // Signal Ports:
    //

    def CreateSignalPorts(
        trunkKey: Trunk.Key,
        interfaceKey: SignalInterface.Key,
        constructors: Vector[SignalPort.Constructor]): Vector[SignalPort] =
    {
        // Get trunk (validate):
        val trunk = GetTrunk(trunkKey)

        // Determine effective interface key:
        val effectiveInterfaceKey =
            if (interfaceKey == SignalInterface.kAnyKey)
                trunk.GetDefaultSignalInterfaceKey
            else
                interfaceKey

        // Get interface:
        val interface = _trunkModel.GetSignalInterfaceOpt(trunk.GetKey, effectiveInterfaceKey).getOrElse(
            throw TrunkException(Cell.ErrorCodes.SignalInterfaceUnknown))

        // Iterate constructors creating signal ports:
        constructors.map(constructor =>
        {
            _signalPortPlant.CreateSignalPort(
                trunk,
                interface,
                constructor)
        })
    }

    def DestroySignalPorts(
        trunkKey: Trunk.Key,
        destructors: Vector[SignalPort.Destructor]): Unit =
    {
        // Get trunk (validate):
        val trunk = GetTrunk(trunkKey)

        if (destructors.isEmpty)
            throw TrunkException(Cell.ErrorCodes.SignalPortDestructorInvalid)
        else
        {
            // Iterate destructors:
            destructors.foreach(destructor =>
            {
                if (destructor._key == SignalPort.kAnyKey)
                {
                    // Destroy all sources:
                    _signalPortPlant.DestroyAllSignalPorts(trunk)
                }
                else
                {
                    // Get port (validate):
                    GetSignalPortOpt (trunk.GetKey, destructor._key)

                    // Destroy port:
                    _signalPortPlant.DestroySignalPort(trunk, destructor)
                }
            })
        }
    }

    def GetSignalPortOpt (
        trunkKey: Trunk.Key,
        key: SignalPort.Key,
        isRequired: Boolean = true): Option[SignalPort] =
    {
        // Get trunk (validate):
        val trunk = GetTrunk(trunkKey)

        // Return port:
        _signalPortPlant.GetSignalPortOpt(
            trunk,
            key,
            isRequired)
    }

    def LookupSignalPort(
        trunkKey: Trunk.Key,
        lookupAlias: String): JsObject =
    {
        // Get trunk (validate):
        val trunk = GetTrunk(trunkKey)

        val result = _signalPortPlant.LookupSignalPort(trunk, lookupAlias) match
        {
            case Some(portKey) => TrunkElement.EncodeKey(portKey)

            case None => ""
        }
        Json.obj(lookupAlias -> result)
    }

    def ReportSignalPorts(
        trunkKey: Trunk.Key,
        interfaceKey: SignalInterface.Key,
        query: SignalPort.Query): Vector[JsObject] =
    {
        // Get trunk (validate):
        val trunk = GetTrunk(trunkKey)

        CommonReporter[SignalPort.Key](
            trunkKey,
            query,
            {
                if (interfaceKey == SignalInterface.kAnyKey || query.keys.isEmpty)
                    _signalPortPlant.GetSignalPortKeys(trunk).toSet
                else
                {
                    GetSignalInterfaceOpt(trunkKey, interfaceKey) match
                    {
                        case Some(interface) => interface.GetPortKeys.toSet

                        case None => throw TrunkException(Cell.ErrorCodes.SignalInterfaceUnknown)
                    }

                }
            },
            GetSignalPortOpt)
    }

    def UpdateSignalPorts(
        trunkKey: Trunk.Key,
        updates: Vector[SignalPort.Update]): Unit =
    {
        // Get trunk (validate):
        val trunk = GetTrunk(trunkKey)

        // Iterate updates:
        updates.foreach(update =>
        {
            _signalPortPlant.GetSignalPortOpt(trunk, update._key) match
            {
                // If port is known, update it:
                case Some(port) =>
                    // Update name:
                    update._nameOpt.foreach(name => port.SetNameOpt(Some(name)))

                    // Update description:
                    update._descriptionOpt.foreach(description => port.SetDescriptionOpt(Some(description)))

                    // Update alias:
                    update._aliasOpt.foreach(alias => port.SetAliasOpt(Some(alias)))

                    // Update signal:
                    if (update._signalEncodedOpt.isDefined)
                    {
                        port.GetInputKeyOpt match
                        {
                            case Some(inputKey) =>
                                _signalInputPlant.GetSignalInputOpt (trunk, inputKey) match
                                {
                                    case Some (input) =>
                                        val signalEncoded = update._signalEncodedOpt.get
                                        val signal = Signal.DecodeSignal (trunk, signalEncoded, port.GetMode)
                                        input.PutSignal (signal)

                                    case None => // ERROR!
                                }

                            case None =>
                                // WARNING
                        }
                    }

                case None => throw TrunkException(Cell.ErrorCodes.SignalPortUnknown)
            }
        })
    }

    //
    // Signal Sources:
    //

    def CreateSignalSources(
        trunkKey: Trunk.Key,
        constructors: Vector[SignalSource.Constructor]): Vector[SignalSource] =
    {
        CommonCreater[SignalSource.Constructor, SignalSource](
            trunkKey,
            constructors,
            _signalSourcePlant.CreateSignalSource)
    }

    def DestroySignalSources(
        trunkKey: Trunk.Key,
        destructors: Vector[SignalSource.Destructor]): Unit =
    {
        // Get trunk (validate):
        val trunk = GetTrunk(trunkKey)

        if (destructors.isEmpty)
            throw TrunkException(Cell.ErrorCodes.SignalSourceDestructorInvalid)
        else
        {
            if (destructors.nonEmpty)
            {
                val markingSignal = _trunkModel.CreateSignal[Signal.Virtual](())

                // Iterate destructors:
                destructors.foreach(destructor =>
                {
                    val sourceKeys: Vector[SignalSource.Key] =
                        if (destructor._key == SignalSource.kAnyKey)
                            _signalSourcePlant.GetSignalSourceKeys(trunk)
                        else
                            Vector(destructor._key)

                    if (sourceKeys.nonEmpty)
                    {
                        sourceKeys.foreach(key =>
                        {
                            GetSignalSourceOpt(
                                trunk.GetKey,
                                key,
                                isRequired = false) match
                            {
                                case Some(source) => source.MarkPathSegment(markingSignal)

                                case None =>    // Ok, possibly propagation-killed already.
                            }
                        })

                        _cell.AddKillOrder(Cell.KillOrder(trunk.GetKey, markingSignal._ordinal))
                    }
                })
            }
        }
    }

    def GetSignalSourceOpt (
        trunkKey: Trunk.Key,
        key: SignalSource.Key,
        isRequired: Boolean = true): Option[SignalSource] =
    {
        // Get trunk (validate):
        val trunk = GetTrunk(trunkKey)

        // Return source option:
        _signalSourcePlant.GetSignalSourceOpt(
            trunk,
            key,
            isRequired)
    }

    def ReportSignalSources (
        trunkKey: Trunk.Key,
        query: SignalSource.Query): Vector[JsObject] =
    {
        // Get trunk (validate):
        val trunk = GetTrunk(trunkKey)

        CommonReporter[SignalSource.Key](
            trunkKey,
            query,
            trunk.GetSignalSourceKeys,
            GetSignalSourceOpt)
    }

    def UpdateSignalSources(
        trunkKey: Trunk.Key,
        updates: Vector[SignalSource.Update]): Unit =
    {
        // Get trunk (validate):
        val trunk = GetTrunk(trunkKey)

        // Iterate updates:
        updates.foreach(update =>
        {
            _signalSourcePlant.GetSignalSourceOpt(trunk, update._key) match
            {
                // If source is known, update it:
                case Some(source) =>
                    // Update name:
                    update._nameOpt.foreach(name => source.SetNameOpt(Some(name)))

                    // Update description:
                    update._descriptionOpt.foreach(description => source.SetDescriptionOpt(Some(description)))

//                    // Update source link:
//                    if (update.linkKeyOpt.isDefined)
//                        source.BindLink(isReciprocal = false, update.linkKeyOpt.get)

//                    // Update source tap:
//                    if (update.tapKeyOpt.isDefined)
//                        source.BindTap(isReciprocal = false, update.tapKeyOpt.get)

                    // Update signal:
                    if (update._signalEncodedOpt.isDefined)
                    {
                        val signalEncoded = update._signalEncodedOpt.get
                        val signal = Signal.DecodeSignal(trunk, signalEncoded, source.GetMode)
                        source.PropagateTest(signal)
                    }

                case None => throw TrunkException(Cell.ErrorCodes.SignalSourceUnknown)
            }
        })
    }

    //
    // Signal Sinks:
    //

    def CreateSignalSinks(
        trunkKey: Trunk.Key,
        constructors: Vector[SignalSink.Constructor],
        listenerOpt: Option[ListenerElement] = None): Vector[SignalSink] =
    {
        // Get trunk (validate):
        val trunk = GetTrunk(trunkKey)

        // Iterate constructors creating signal sinks:
        constructors.map(constructor =>
        {
            _signalSinkPlant.CreateSignalSink(trunk, constructor, listenerOpt)
        })
    }

    def DestroySignalSinks(
        trunkKey: Trunk.Key,
        destructors: Vector[SignalSink.Destructor]): Unit =
    {
        // Get trunk (validate):
        val trunk = GetTrunk(trunkKey)

        if (destructors.isEmpty)
            throw TrunkException(Cell.ErrorCodes.SignalSinkDestructorInvalid)
        else
        {
            if (destructors.nonEmpty)
            {
                val markingSignal = _trunkModel.CreateSignal[Signal.Virtual](())

                // Iterate destructors:
                destructors.foreach(destructor =>
                {
                    val sinkKeys: Vector[SignalSink.Key] =
                        if (destructor._key == SignalSink.kAnyKey)
                            _signalSinkPlant.GetSignalSinkKeys(trunk)
                        else
                            Vector(destructor._key)

                    if (sinkKeys.nonEmpty)
                    {
                        sinkKeys.foreach(key =>
                        {
                            GetSignalSinkOpt(
                                trunk.GetKey,
                                key,
                                isRequired = false) match
                            {
                                case Some(sink) =>
                                    // Pre-mark all of the sink's links for kill:
                                    sink.MarkAllLinks(markingSignal)

                                    // Mark sink's propagation path for kill:
                                    sink.MarkPathSegment(markingSignal)

                                case None =>    // Ok, possibly propagation-killed already.
                            }
                        })

                        _cell.AddKillOrder(Cell.KillOrder(trunk.GetKey, markingSignal._ordinal))
                    }
                })
            }
        }
    }

    def GetSignalSinkOpt (
        trunkKey: Trunk.Key,
        key: SignalSink.Key,
        isRequired: Boolean = true): Option[SignalSink] =
    {
        // Get trunk (validate):
        val trunk = GetTrunk(trunkKey)

        // Return sink option:
        _signalSinkPlant.GetSignalSinkOpt(
            trunk,
            key,
            isRequired)
    }

    def ReportSignalSinks(
        trunkKey: Trunk.Key,
        query: SignalSink.Query): Vector[JsObject] =
    {
        // Get trunk (validate):
        val trunk = GetTrunk(trunkKey)

        CommonReporter[SignalSink.Key](
            trunkKey,
            query,
            trunk.GetSignalSinkKeys,
            GetSignalSinkOpt)
    }

    def UpdateSignalSinks(
        trunkKey: Trunk.Key,
        updates: Vector[SignalSink.Update]): Unit =
    {
        // Get trunk (validate):
        val trunk = GetTrunk(trunkKey)

        // Iterate updates:
        updates.foreach(update =>
        {
            _signalSinkPlant.GetSignalSinkOpt(trunk, update._key) match
            {
                // If sink is known, update it:
                case Some(sink) =>
                    // Update name:
                    update._nameOpt.foreach(name => sink.SetNameOpt(Some(name)))

                    // Update description:
                    update._descriptionOpt.foreach(description => sink.SetDescriptionOpt(Some(description)))

//                    // Update sink link:
//                    if (update.linkKeyOpt.isDefined)
//                        sink.BindLink(isReciprocal = false, update.linkKeyOpt.get)
//
//                    // Update sink tap:
//                    if (update.tapKeyOpt.isDefined)
//                        sink.BindTap(isReciprocal = false, update.tapKeyOpt.get)

                    // Update signal:
                    if (update._signalEncodedOpt.isDefined)
                    {
                        val signalEncoded = update._signalEncodedOpt.get
                        val signal = Signal.DecodeSignal(trunk, signalEncoded, sink.GetMode)
                        sink.PropagateTest(signal)
                    }

                case None => throw TrunkException(Cell.ErrorCodes.SignalSinkUnknown)
            }
        })
    }

    //
    // Signal Links:
    //

    def CreateSignalLinks(
        trunkKey: Trunk.Key,
        constructors: Vector[SignalLink.Constructor]): Vector[SignalLink] =
    {
        CommonCreater[SignalLink.Constructor, SignalLink](
            trunkKey,
            constructors,
            _signalLinkPlant.CreateSignalLink)
    }

    def DestroySignalLinks(
        trunkKey: Trunk.Key,
        destructors: Vector[SignalLink.Destructor]): Unit =
    {
        // Get trunk (validate):
        val trunk = GetTrunk(trunkKey)

        if (destructors.isEmpty)
            throw TrunkException(Cell.ErrorCodes.SignalLinkDestructorInvalid)
        else
        {
            if (destructors.nonEmpty)
            {
                val markingSignal = _trunkModel.CreateSignal[Signal.Virtual](())

                // Iterate destructors:
                destructors.foreach(destructor =>
                {
                    val linkKeys: Vector[SignalLink.Key] =
                        if (destructor._key == SignalLink.kAnyKey)
                            _signalLinkPlant.GetSignalLinkKeys(trunk)
                        else
                            Vector(destructor._key)

                    if (linkKeys.nonEmpty)
                    {
                        linkKeys.foreach(key =>
                        {
                            GetSignalLinkOpt(
                                trunk.GetKey,
                                key,
                                isRequired = false) match
                            {
                                case Some(link) => link.MarkPathSegment(markingSignal)

                                case None =>    // Ok, possibly propagation-killed already.
                            }
                        })

                        _cell.AddKillOrder(Cell.KillOrder(trunk.GetKey, markingSignal._ordinal))
                    }
                })
            }
        }
    }

    def GetSignalLinkOpt (
        trunkKey: Trunk.Key,
        key: SignalLink.Key,
        isRequired: Boolean = true): Option[SignalLink] =
    {
        // Get trunk (validate):
        val trunk = GetTrunk(trunkKey)

        // Return link option:
        _signalLinkPlant.GetSignalLinkOpt(
            trunk,
            key,
            isRequired)
    }

    def ReportSignalLinks(
        trunkKey: Trunk.Key,
        query: SignalLink.Query): Vector[JsObject] =
    {
        // Get trunk (validate):
        val trunk = GetTrunk(trunkKey)

        CommonReporter[SignalLink.Key](
            trunkKey,
            query,
            trunk.GetSignalLinkKeys,
            GetSignalLinkOpt)
    }

    def UpdateSignalLinks(
        trunkKey: Trunk.Key,
        updates: Vector[SignalLink.Update]): Unit =
    {
        // Get trunk (validate):
        val trunk = GetTrunk(trunkKey)

        // Iterate updates:
        updates.foreach(update =>
        {
            _signalLinkPlant.GetSignalLinkOpt(trunk, update._key) match
            {
                // If link is known, update it:
                case Some(link) =>
                    // Update name:
                    update._nameOpt.foreach(name => link.SetNameOpt(Some(name)))

                    // Update description:
                    update._descriptionOpt.foreach(description => link.SetDescriptionOpt(Some(description)))

                    // Update signal:
                    if (update._signalEncodedOpt.isDefined)
                    {
                        val signalEncoded = update._signalEncodedOpt.get
                        val signal = Signal.DecodeSignal(trunk, signalEncoded, link.GetMode)
                        link.PropagateTest(signal)
                    }

                case None => throw TrunkException(Cell.ErrorCodes.SignalLinkUnknown)
            }
        })
    }

    //
    // Signal Taps:
    //

    def CreateSignalTaps(
        trunkKey: Trunk.Key,
        constructors: Vector[SignalTap.Constructor]): Vector[SignalTap] =
    {
        CommonCreater[SignalTap.Constructor, SignalTap](
            trunkKey,
            constructors,
            _signalTapPlant.CreateSignalTap)
    }

    def DestroySignalTaps(
        trunkKey: Trunk.Key,
        destructors: Vector[SignalTap.Destructor]): Unit =
    {
        // Get trunk (validate):
        val trunk = GetTrunk(trunkKey)

        if (destructors.isEmpty)
            throw TrunkException(Cell.ErrorCodes.SignalTapDestructorInvalid)
        else
        {
            if (destructors.nonEmpty)
            {
                val markingSignal = _trunkModel.CreateSignal[Signal.Virtual](())

                // Iterate destructors:
                destructors.foreach(destructor =>
                {
                    val tapKeys: Vector[SignalTap.Key] =
                        if (destructor._key == SignalTap.kAnyKey)
                            _signalTapPlant.GetSignalTapKeys(trunk)
                        else
                            Vector(destructor._key)

                    if (tapKeys.nonEmpty)
                    {
                        tapKeys.foreach(key =>
                        {
                            GetSignalTapOpt(
                                trunk.GetKey,
                                key,
                                isRequired = false) match
                            {
                                case Some(tap) =>
                                    // Mark path segment from sink if it still exists, else from tap:
                                    GetSignalSinkOpt(trunkKey, tap.GetSinkKey) match
                                    {
                                        case Some(sink) => sink.MarkPathSegment(markingSignal)

                                        case None => tap.MarkPathSegment(markingSignal)
                                    }

                                case None =>    // Ok, possibly propagation-killed already.
                            }
                        })

                        _cell.AddKillOrder(Cell.KillOrder(trunk.GetKey, markingSignal._ordinal))
                    }
                })
            }
        }
    }

    def GetSignalTapOpt (
        trunkKey: Trunk.Key,
        key: SignalTap.Key,
        isRequired: Boolean = true): Option[SignalTap] =
    {
        // Get trunk (validate):
        val trunk = GetTrunk(trunkKey)

        // Return tap option:
        _signalTapPlant.GetSignalTapOpt(
            trunk,
            key,
            isRequired)
    }

    def ReportSignalTaps(
        trunkKey: Trunk.Key,
        query: SignalTap.Query): Vector[JsObject] =
    {
        // Get trunk (validate):
        val trunk = GetTrunk(trunkKey)

        CommonReporter[SignalTap.Key](
            trunkKey,
            query,
            trunk.GetSignalTapKeys,
            GetSignalTapOpt)
    }

    def UpdateSignalTaps(
        trunkKey: Trunk.Key,
        updates: Vector[SignalTap.Update]): Unit =
    {
        // Get trunk (validate):
        val trunk = GetTrunk(trunkKey)

        // Iterate updates:
        updates.foreach(update =>
        {
            _signalTapPlant.GetSignalTapOpt(trunk, update._key) match
            {
                // If tap is known, update it:
                case Some(tap) =>
                    // Update name:
                    update._nameOpt.foreach(name => tap.SetNameOpt(Some(name)))

                    // Update description:
                    update._descriptionOpt.foreach(description => tap.SetDescriptionOpt(Some(description)))

                    // Update signal:
                    if (update._signalEncodedOpt.isDefined)
                    {
                        val signalEncoded = update._signalEncodedOpt.get
                        val signal = Signal.DecodeSignal(trunk, signalEncoded, tap.GetMode)
                        tap.PropagateTest(signal)
                    }

                case None => throw TrunkException(Cell.ErrorCodes.SignalTapUnknown)
            }
        })
    }

    //
    // Signal Inputs:
    //

    def CreateSignalInputs(
        trunkKey: Trunk.Key,
        constructors: Vector[SignalInput.Constructor]): Vector[SignalInput] =
    {
        CommonCreater[SignalInput.Constructor, SignalInput](
            trunkKey,
            constructors,
            _signalInputPlant.CreateSignalInput)
    }

    def DestroySignalInputs(
        trunkKey: Trunk.Key,
        destructors: Vector[SignalInput.Destructor]): Unit =
    {
        // Get trunk (validate):
        val trunk = GetTrunk(trunkKey)

        if (destructors.isEmpty)
            throw TrunkException(Cell.ErrorCodes.SignalInputDestructorInvalid)
        else
        {
            // Iterate destructors:
            destructors.foreach(destructor =>
            {
                if (destructor._key == SignalInput.kAnyKey)
                {
                    // Destroy all inputs:
                    _signalInputPlant.DestroyAllSignalInputs(trunk)
                }
                else
                {
                    // Get input (validate):
                    GetSignalInputOpt(trunk.GetKey, destructor._key)

                    // Destroy input:
                    _signalInputPlant.DestroySignalInput(trunk, destructor)
                }
            })
        }
    }

    def GetSignalInputOpt (
        trunkKey: Trunk.Key,
        key: SignalInput.Key,
        isRequired: Boolean = true): Option[SignalInput] =
    {
        // Get trunk (validate):
        val trunk = GetTrunk(trunkKey)

        // Return input:
        _signalInputPlant.GetSignalInputOpt(
            trunk,
            key,
            isRequired)
    }

    def ReportSignalInputs(
        trunkKey: Trunk.Key,
        query: SignalInput.Query): Vector[JsObject] =
    {
        // Get trunk (validate):
        val trunk = GetTrunk(trunkKey)

        CommonReporter[SignalInput.Key](
            trunkKey,
            query,
            trunk.GetSignalInputKeys,
            GetSignalInputOpt)
    }

    def UpdateSignalInputs(
        trunkKey: Trunk.Key,
        updates: Vector[SignalInput.Update]): Unit =
    {
        // Get trunk (validate):
        val trunk = GetTrunk(trunkKey)

        // Iterate updates:
        updates.foreach(update =>
        {
            _signalInputPlant.GetSignalInputOpt(trunk, update._key) match
            {
                // If input is known, update it:
                case Some(input) =>
                    // Update name:
                    update._nameOpt.foreach(name => input.SetNameOpt(Some(name)))

                    // Update description:
                    update._descriptionOpt.foreach(description => input.SetDescriptionOpt(Some(description)))

                    // Update signal:
                    if (update._signalEncodedOpt.isDefined)
                    {
                        val signalEncoded = update._signalEncodedOpt.get
                        val signal = Signal.DecodeSignal(trunk, signalEncoded, input.GetMode)
                        input.PropagateTest(signal)
                    }

                case None => throw TrunkException(Cell.ErrorCodes.SignalInputUnknown)
            }
        })
    }

    //
    // Signal Bridges:
    //

    def CreateSignalBridges(
        trunkKey: Trunk.Key,
        constructors: Vector[SignalBridge.Constructor]): Vector[SignalBridge] =
    {
        CommonCreater[SignalBridge.Constructor, SignalBridge](
            trunkKey,
            constructors,
            _signalBridgePlant.CreateSignalBridge)
    }

    def DestroySignalBridges(
        trunkKey: Trunk.Key,
        destructors: Vector[SignalBridge.Destructor]): Unit =
    {
        // Get trunk (validate):
        val trunk = GetTrunk(trunkKey)

        if (destructors.isEmpty)
            throw TrunkException(Cell.ErrorCodes.SignalBridgeDestructorInvalid)
        else
        {
            // Iterate destructors:
            destructors.foreach(destructor =>
            {
                if (destructor._key == SignalBridge.kAnyKey)
                {
                    // Destroy all bridges:
                    _signalBridgePlant.DestroyAllSignalBridges(trunk)
                }
                else
                {
                    // Get bridge (validate):
                    GetSignalBridgeOpt(trunk.GetKey, destructor._key)

                    // Destroy bridge:
                    _signalBridgePlant.DestroySignalBridge(trunk, destructor)
                }
            })
        }
    }

    def GetSignalBridgeOpt (
        trunkKey: Trunk.Key,
        key: SignalBridge.Key,
        isRequired: Boolean = true): Option[SignalBridge] =
    {
        // Get trunk (validate):
        val trunk = GetTrunk(trunkKey)

        // Return bridge:
        _signalBridgePlant.GetSignalBridgeOpt(
            trunk,
            key,
            isRequired)
    }

    def ReportSignalBridges(
        trunkKey: Trunk.Key,
        query: SignalBridge.Query): Vector[JsObject] =
    {
        // Get trunk (validate):
        val trunk = GetTrunk(trunkKey)

        CommonReporter[SignalBridge.Key](
            trunkKey,
            query,
            trunk.GetSignalBridgeKeys,
            GetSignalBridgeOpt)
    }

    def UpdateSignalBridges(
        trunkKey: Trunk.Key,
        updates: Vector[SignalBridge.Update]): Unit =
    {
        // Get trunk (validate):
        val trunk = GetTrunk(trunkKey)

        // Iterate updates:
        updates.foreach(update =>
        {
            _signalBridgePlant.GetSignalBridgeOpt(trunk, update._key) match
            {
                // If bridge is known, update it:
                case Some(bridge) =>
                    // Update name:
                    update._nameOpt.foreach(name => bridge.SetNameOpt(Some(name)))

                    // Update description:
                    update._descriptionOpt.foreach(description => bridge.SetDescriptionOpt(Some(description)))

                    // Update signal:
                    if (update._signalEncodedOpt.isDefined)
                    {
                        val signalEncoded = update._signalEncodedOpt.get
                        val signal = Signal.DecodeSignal(trunk, signalEncoded, bridge.GetMode)
                        bridge.PropagateTest(signal)
                    }

                case None => throw TrunkException(Cell.ErrorCodes.SignalBridgeUnknown)
            }
        })
    }

    //
    // Signal Outputs:
    //

    def CreateSignalOutputs(
        trunkKey: Trunk.Key,
        constructors: Vector[SignalOutput.Constructor]): Vector[SignalOutput] =
    {
        CommonCreater[SignalOutput.Constructor, SignalOutput](
            trunkKey,
            constructors,
            _signalOutputPlant.CreateSignalOutput)
    }

    def DestroySignalOutputs(
        trunkKey: Trunk.Key,
        destructors: Vector[SignalOutput.Destructor]): Unit =
    {
        // Get trunk (validate):
        val trunk = GetTrunk(trunkKey)

        if (destructors.isEmpty)
            throw TrunkException(Cell.ErrorCodes.SignalOutputDestructorInvalid)
        else
        {
            // Iterate destructors:
            destructors.foreach(destructor =>
            {
                if (destructor._key == SignalOutput.kAnyKey)
                {
                    // Destroy all outputs:
                    _signalOutputPlant.DestroyAllSignalOutputs(trunk)
                }
                else
                {
                    // Get output (validate):
                    GetSignalOutputOpt(trunk.GetKey, destructor._key)

                    // Destroy output:
                    _signalOutputPlant.DestroySignalOutput(trunk, destructor)
                }
            })
        }
    }

    def GetSignalOutputOpt (
        trunkKey: Trunk.Key,
        key: SignalOutput.Key,
        isRequired: Boolean = true): Option[SignalOutput] =
    {
        // Get trunk (validate):
        val trunk = GetTrunk(trunkKey)

        // Return output:
        _signalOutputPlant.GetSignalOutputOpt(
            trunk,
            key,
            isRequired)
    }

    def ReportSignalOutputs(
        trunkKey: Trunk.Key,
        query: SignalOutput.Query): Vector[JsObject] =
    {
        // Get trunk (validate):
        val trunk = GetTrunk(trunkKey)

        CommonReporter[SignalOutput.Key](
            trunkKey,
            query,
            trunk.GetSignalOutputKeys,
            GetSignalOutputOpt)
    }

    def UpdateSignalOutputs(
        trunkKey: Trunk.Key,
        updates: Vector[SignalOutput.Update]): Unit =
    {
        // Get trunk (validate):
        val trunk = GetTrunk(trunkKey)

        // Iterate updates:
        updates.foreach(update =>
        {
            _signalOutputPlant.GetSignalOutputOpt(trunk, update._key) match
            {
                // If output is known, update it:
                case Some(output) =>
                    // Update name:
                    update._nameOpt.foreach(name => output.SetNameOpt(Some(name)))

                    // Update description:
                    update._descriptionOpt.foreach(description => output.SetDescriptionOpt(Some(description)))

                    // Update signal:
                    if (update._signalEncodedOpt.isDefined)
                    {
                        val signalEncoded = update._signalEncodedOpt.get
                        val signal = Signal.DecodeSignal(trunk, signalEncoded, output.GetMode)
                        output.PropagateTest(signal)
                    }

                case None => throw TrunkException(Cell.ErrorCodes.SignalOutputUnknown)
            }
        })
    }

    //
    // Emitter Patches:
    //

    def CreateEmitterPatches(
        trunkKey: Trunk.Key,
        constructors: Vector[EmitterPatch.Constructor]): Vector[EmitterPatch] =
    {
        CommonCreater[EmitterPatch.Constructor, EmitterPatch](
            trunkKey,
            constructors,
            _emitterPatchPlant.CreateEmitterPatch)
    }

    def DestroyEmitterPatches(
        trunkKey: Trunk.Key,
        destructors: Vector[EmitterPatch.Destructor]): Unit =
    {
        // Get trunk (validate):
        val trunk = GetTrunk(trunkKey)

        if (destructors.isEmpty)
            throw TrunkException(Cell.ErrorCodes.EmitterPatchDestructorInvalid)
        else
        {
            // Iterate destructors:
            destructors.foreach(destructor =>
            {
                if (destructor._key == EmitterPatch.kAnyKey)
                {
                    // Destroy all patches:
                    _emitterPatchPlant.DestroyAllEmitterPatches(trunk)
                }
                else
                {
                    // Get patch (validate):
                    GetEmitterPatchOpt(trunk.GetKey, destructor._key)

                    // Destroy patch:
                    _emitterPatchPlant.DestroyEmitterPatch(trunk, destructor)
                }
            })
        }
    }

    def GetEmitterPatchOpt (
        trunkKey: Trunk.Key,
        key: EmitterPatch.Key,
        isRequired: Boolean = true): Option[EmitterPatch] =
    {
        // Get trunk (validate):
        val trunk = GetTrunk(trunkKey)

        // Return patch:
        _emitterPatchPlant.GetEmitterPatchOpt(
            trunk,
            key,
            isRequired)
    }

    def ReportEmitterPatches(
        trunkKey: Trunk.Key,
        query: EmitterPatch.Query): Vector[JsObject] =
    {
        // Get trunk (validate):
        val trunk = GetTrunk(trunkKey)

        CommonReporter[EmitterPatch.Key](
            trunkKey,
            query,
            trunk.GetEmitterPatchKeys,
            GetEmitterPatchOpt)
    }

    def ReportPatchOfFieldEmitter (
        trunkKey: Trunk.Key,
        query: EmitterPatch.Query): Vector[JsObject] =
    {
        // Get trunk (validate):
        val trunk = GetTrunk(trunkKey)

        CommonReporter[EmitterPatch.Key](
            trunkKey,
            query,
            trunk.GetEmitterPatchKeys,
            GetEmitterPatchOpt)
    }

    def UpdateEmitterPatches(
        trunkKey: Trunk.Key,
        updates: Vector[EmitterPatch.Update]): Unit =
    {
        // Get trunk (validate):
        val trunk = GetTrunk(trunkKey)

        // Iterate updates:
        updates.foreach(update =>
        {
            _emitterPatchPlant.GetEmitterPatchOpt(trunk, update._key) match
            {
                // If patch is known, update it:
                case Some(patch) =>
                    // Update name:
                    update._nameOpt.foreach(name => patch.SetNameOpt(Some(name)))

                    // Update description:
                    update._descriptionOpt.foreach(description => patch.SetDescriptionOpt(Some(description)))

                    // Update patch def:
                    if (update._patchDefOpt.isDefined)
                    {
                        // Deploy the patch:
                        val fieldModel = GetFieldModel
                        val field = fieldModel.GetField(patch.GetFieldKey)
                        patch.GetEmitterKey match
                        {
                            case fieldEmitterKey: FieldEmitter.Key =>
                                fieldModel.GetFieldEmitterPlant.DeployEmitterPatch(
                                    field,
                                    fieldModel.GetFieldEmitterOpt(patch.GetFieldKey, fieldEmitterKey).get,
                                    update._patchDefOpt)

                            case subjectEmitterKey: SubjectEmitter.Key =>
                                fieldModel.GetSubjectEmitterPlant.DeployEmitterPatch(
                                    field,
                                    fieldModel.GetSubjectEmitterOpt(patch.GetFieldKey, subjectEmitterKey).get,
                                    update._patchDefOpt)

                            case probeEmitterKey: ProbeEmitter.Key =>
                                fieldModel.GetProbeEmitterPlant.DeployEmitterPatch(
                                    field,
                                    fieldModel.GetProbeEmitterOpt(patch.GetFieldKey, probeEmitterKey).get,
                                    update._patchDefOpt)
                        }
                    }

                    // Update signal:
                    if (update._signalEncodedOpt.isDefined)
                    {
                        val signalEncoded = update._signalEncodedOpt.get
                        val signal = Signal.DecodeSignal(trunk, signalEncoded, patch.GetMode)
                        patch.PropagateTest(signal)
                    }

                case None => throw TrunkException(Cell.ErrorCodes.EmitterPatchUnknown)
            }
        })
    }

    //
    // Oscillator Patches:
    //

    def CreateOscillatorPatches (
        trunkKey: Trunk.Key,
        constructors: Vector[OscillatorPatch.Constructor]): Vector[OscillatorPatch] =
    {
        CommonCreater[OscillatorPatch.Constructor, OscillatorPatch](
            trunkKey,
            constructors,
            _oscillatorPatchPlant.CreateOscillatorPatch)
    }

    def DestroyOscillatorPatches(
        trunkKey: Trunk.Key,
        destructors: Vector[OscillatorPatch.Destructor]): Unit =
    {
        // Get trunk (validate):
        val trunk = GetTrunk(trunkKey)

        if (destructors.isEmpty)
            throw TrunkException(Cell.ErrorCodes.OscillatorPatchDestructorInvalid)
        else
        {
            // Iterate destructors:
            destructors.foreach(destructor =>
            {
                if (destructor._key == OscillatorPatch.kAnyKey)
                {
                    // Destroy all patches:
                    _oscillatorPatchPlant.DestroyAllOscillatorPatches(trunk)
                }
                else
                {
                    // Get patch (validate):
                    GetOscillatorPatchOpt(trunk.GetKey, destructor._key)

                    // Destroy patch:
                    _oscillatorPatchPlant.DestroyOscillatorPatch(trunk, destructor)
                }
            })
        }
    }

    def GetOscillatorPatchOpt (
        trunkKey: Trunk.Key,
        key: OscillatorPatch.Key,
        isRequired: Boolean = true): Option[OscillatorPatch] =
    {
        // Get trunk (validate):
        val trunk = GetTrunk(trunkKey)

        // Return patch:
        _oscillatorPatchPlant.GetOscillatorPatchOpt(
            trunk,
            key,
            isRequired)
    }

    def ReportOscillatorPatches(
        trunkKey: Trunk.Key,
        emitterPatchKey: EmitterPatch.Key,
        query: OscillatorPatch.Query): Vector[JsObject] =
    {
        // Get trunk (validate):
        val trunk = GetTrunk(trunkKey)

        CommonReporter[OscillatorPatch.Key](
        trunkKey,
        query,
        {
            if (emitterPatchKey == EmitterPatch.kAnyKey || query.keys.isEmpty)
                _oscillatorPatchPlant.GetOscillatorPatchKeys(trunk).toSet
            else
            {
                GetEmitterPatchOpt(trunkKey, emitterPatchKey) match
                {
                    case Some(emitterPatch) => emitterPatch.GetOscillatorPatchKeysMap.values.toSet

                    case None => throw TrunkException(Cell.ErrorCodes.EmitterPatchUnknown)
                }

            }
        },
        GetOscillatorPatchOpt)
    }

    def UpdateOscillatorPatches(
        trunkKey: Trunk.Key,
        updates: Vector[OscillatorPatch.Update]): Unit =
    {
        // Get trunk (validate):
        val trunk = GetTrunk(trunkKey)

        // Iterate updates:
        updates.foreach(update =>
        {
            _oscillatorPatchPlant.GetOscillatorPatchOpt(trunk, update._key) match
            {
                // If patch is known, update it:
                case Some(patch) =>
                    // Update name:
                    update._nameOpt.foreach(name => patch.SetNameOpt(Some(name)))

                    // Update description:
                    update._descriptionOpt.foreach(description => patch.SetDescriptionOpt(Some(description)))

                    // Update patch def:
                    if (update._patchDefOpt.isDefined)
                    {
                        // Tell patch to import new patch definition:
                        _logger.LogDebug(s"UOP: importing oscillator patch definition: $update.patchDefOpt")
                        patch.ImportOscillatorPatchDef(update._patchDefOpt)
                    }

                    // Update signal:
                    if (update._signalEncodedOpt.isDefined)
                    {
                        val signalEncoded = update._signalEncodedOpt.get
                        val signal = Signal.DecodeSignal(trunk, signalEncoded, patch.GetMode)
                        patch.PropagateTest(signal)
                    }

                case None => throw TrunkException(Cell.ErrorCodes.OscillatorPatchUnknown)
            }
        })
    }

    //
    // Oscillator Patch Envelopes
    //

    def ReportOscillatorPatchEnvelopes (
        trunkKey: Trunk.Key,
        oscillatorPatchKey: OscillatorPatch.Key,
        query: OscillatorEnvelope.Query): Vector[JsObject] =
    {
        // Get trunk (validate):
        GetTrunk(trunkKey)

        // Get envelope's patch:
        val patch: OscillatorPatch = GetOscillatorPatchOpt(trunkKey, oscillatorPatchKey).get

        // Calculate valid keys as intersect of requested keys and keys known by field:
        val knownKeys = Set(
            new OscillatorEnvelope.Key(FieldModel.Glossary.kQLoudness),
            new OscillatorEnvelope.Key(FieldModel.Glossary.kQPitch),
            new OscillatorEnvelope.Key(FieldModel.Glossary.kQPeriod),
            new OscillatorEnvelope.Key(FieldModel.Glossary.kQShape),
            new OscillatorEnvelope.Key(FieldModel.Glossary.kQTone))
        val validKeys = if (query._keys.isEmpty) knownKeys else knownKeys.intersect(query.GetKeys.toSet)

        // Iterate valid keys:
        val reports = validKeys.map(key =>
        {
            val reporter = patch.GetEnvelopeReporter(key)
            reporter.Report(query._sectionsOpt)
        })

        // Return reports vector:
        reports.toVector
    }

    def UpdateOscillatorPatchEnvelopes (
        trunkKey: Trunk.Key,
        oscillatorPatchKey: OscillatorPatch.Key,
        updates: Vector[OscillatorEnvelope.Update]): Unit =
    {
        // Get trunk (validate):
        GetTrunk(trunkKey)

        // Get envelope's patch:
        val patch: OscillatorPatch = GetOscillatorPatchOpt(trunkKey, oscillatorPatchKey).get

        // Iterate updates:
        updates.foreach(update =>
        {
            patch.ImportEnvelopeDef(update._key, update._envelopeDef)
        })
    }


    def GetDefaultTrunk: Trunk =
        _defaultTrunkOpt.getOrElse(throw TrunkException(Cell.ErrorCodes.TrunkUnknown))

    def Initialize (isTesting: Boolean = false): Unit =
    {
        val tag = new StringBuilder(if (isTesting) TrunkModel.Glossary.kBang else "")
        tag ++= TrunkModel.Glossary.kEDefaultTrunk

        val trunkConstructor = Trunk.Constructor(_tag = tag.toString())
        _defaultTrunkOpt = Some(_trunkPlant.CreateTrunk(trunkConstructor))
    }

    def Log (text: String): Unit =
        _logger.LogDebug(text)
}
