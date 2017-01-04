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

package org.taranos.mc

import org.taranos.common._
import org.taranos.mc.Cell.{CellReporter, KillOrder}
import org.taranos.mc.Common.Reporter
import org.taranos.mc.field._
import org.taranos.mc.trunk.intraprocess._
import play.api.libs.json._


object Cell
{
    object ErrorCodes
    {
        val Ok                                = 0
        val KeyInvalid                        = -10
        val KeysInvalid                       = -11
        val SignalInvalid                     = -20
        val SignalModeInvalid                 = -21
        val SignalModeIndeterminate           = -22
        val PositionInvalid                   = -50
        val RotationInvalid                   = -51
        val ChannelDefinitionInvalid          = -60
        val EnvelopeDefinitionInvalid         = -61
        val PoleDefinitionInvalid             = -62
        val MacroUnsupported                  = -70
        val MacroInvalid                      = -71
        val FieldKeyInvalid                   = -100
        val FieldUnknown                      = -101
        val FieldInvalid                      = -102
        val FieldUnavailable                  = -103
        val FieldConstructorInvalid           = -104
        val FieldDestructorInvalid            = -105
        val FieldUpdateInvalid                = -106
        val FieldEmitterKeyInvalid            = -110
        val FieldEmitterUnknown               = -111
        val FieldEmitterInvalid               = -112
        val FieldEmitterUnavailable           = -113
        val FieldEmitterConstructorInvalid    = -114
        val FieldEmitterDestructorInvalid     = -115
        val FieldEmitterUpdateInvalid         = -116
        val FieldEmitterCallInvalid           = -117
        val FieldEmitterPatchless             = -118
        val FieldOscillatorKeyInvalid         = -120
        val FieldOscillatorUnknown            = -121
        val FieldOscillatorInvalid            = -122
        val FieldOscillatorUnavailable        = -123
        val FieldOscillatorConstructorInvalid = -124
        val FieldOscillatorDestructorInvalid  = -125
        val FieldOscillatorUpdateInvalid      = -126
        val FieldOscillatorCallInvalid        = -127
        val FieldOscillatorPatchless          = -128
        val SubjectKeyInvalid                 = -130
        val SubjectUnknown                    = -131
        val SubjectInvalid                    = -132
        val SubjectUnavailable                = -133
        val SubjectConstructorInvalid         = -134
        val SubjectDestructorInvalid          = -135
        val SubjectUpdateInvalid              = -136
        val SubjectEmitterKeyInvalid          = -140
        val SubjectEmitterUnknown             = -141
        val SubjectEmitterInvalid             = -142
        val SubjectEmitterUnavailable         = -143
        val SubjectEmitterConstructorInvalid  = -144
        val SubjectEmitterDestructorInvalid   = -145
        val SubjectEmitterUpdateInvalid       = -146
        val SubjectEmitterCallInvalid         = -147
        val SubjectEmitterPatchless           = -148
        val SubjectOscillatorKeyInvalid       = -150
        val SubjectOscillatorUnknown          = -151
        val SubjectOscillatorInvalid          = -152
        val SubjectOscillatorUnavailable      = -153
        val SubjectOscillatorConstructorInvalid = -154
        val SubjectOscillatorDestructorInvalid = -155
        val SubjectOscillatorUpdateInvalid     = -156
        val SubjectOscillatorCallInvalid       = -157
        val SubjectOscillatorPatchless         = -158
        val ProbeKeyInvalid                    = -160
        val ProbeUnknown                       = -161
        val ProbeInvalid                       = -162
        val ProbeUnavailable                   = -163
        val ProbeConstructorInvalid            = -164
        val ProbeDestructorInvalid             = -165
        val ProbeUpdateInvalid                 = -166
        val ProbeEmitterKeyInvalid             = -170
        val ProbeEmitterUnknown                = -171
        val ProbeEmitterInvalid                = -172
        val ProbeEmitterUnavailable            = -173
        val ProbeEmitterConstructorInvalid     = -174
        val ProbeEmitterDestructorInvalid      = -175
        val ProbeEmitterUpdateInvalid          = -176
        val ProbeEmitterCallInvalid            = -177
        val ProbeEmitterPatchless              = -178
        val ProbeOscillatorKeyInvalid          = -180
        val ProbeOscillatorUnknown             = -181
        val ProbeOscillatorInvalid             = -182
        val ProbeOscillatorUnavailable         = -183
        val ProbeOscillatorConstructorInvalid  = -184
        val ProbeOscillatorDestructorInvalid   = -185
        val ProbeOscillatorUpdateInvalid       = -186
        val ProbeOscillatorCallInvalid         = -187
        val ProbeOscillatorPatchless           = -158
        val ProbeCollectorKeyInvalid           = -190
        val ProbeCollectorUnknown              = -191
        val ProbeCollectorInvalid              = -192
        val ProbeCollectorUnavailable          = -193
        val ProbeCollectorConstructorInvalid   = -194
        val ProbeCollectorDestructorInvalid    = -195
        val ProbeCollectorUpdateInvalid        = -196
        val EmitterPatchKeyInvalid             = -300
        val EmitterPatchUnknown                = -301
        val EmitterPatchInvalid                = -302
        val EmitterPatchUnavailable            = -303
        val EmitterPatchConstructorInvalid     = -304
        val EmitterPatchDestructorInvalid      = -305
        val EmitterPatchUpdateInvalid          = -306
        val EmitterPatchTapless                = -307
        val OscillatorPatchKeyInvalid          = -310
        val OscillatorPatchUnknown             = -311
        val OscillatorPatchInvalid             = -312
        val OscillatorPatchUnavailable         = -313
        val OscillatorPatchConstructorInvalid  = -314
        val OscillatorPatchDestructorInvalid   = -315
        val OscillatorPatchUpdateInvalid       = -316
        val OscillatorPatchTapless             = -317
        val OscillatorPatchBroken              = -318
        val SignalInputKeyInvalid              = -330
        val SignalInputUnknown                 = -331
        val SignalInputInvalid                 = -332
        val SignalInputUnavailable             = -333
        val SignalInputConstructorInvalid      = -334
        val SignalInputDestructorInvalid       = -335
        val SignalInputUpdateInvalid           = -336
        val SignalInputTapless                 = -337
        val SignalBridgeKeyInvalid             = -340
        val SignalBridgeUnknown                = -341
        val SignalBridgeInvalid                = -342
        val SignalBridgeUnavailable            = -343
        val SignalBridgeConstructorInvalid     = -344
        val SignalBridgeDestructorInvalid      = -345
        val SignalBridgeUpdateInvalid          = -346
        val SignalBridgeTapless                = -347
        val SignalOutputKeyInvalid             = -350
        val SignalOutputUnknown                = -351
        val SignalOutputInvalid                = -352
        val SignalOutputUnavailable            = -353
        val SignalOutputConstructorInvalid     = -354
        val SignalOutputDestructorInvalid      = -355
        val SignalOutputUpdateInvalid          = -356
        val SignalOutputTapless                = -357
        val SignalOutputBroken                 = -358
        val TrunkKeyInvalid                    = -400
        val TrunkUnknown                       = -401
        val TrunkInvalid                       = -402
        val TrunkUnavailable                   = -403
        val TrunkConstructorInvalid            = -404
        val TrunkDestructorInvalid             = -405
        val TrunkUpdateInvalid                 = -406
        val SignalInterfaceKeyInvalid          = -410
        val SignalInterfaceUnknown             = -411
        val SignalInterfaceInvalid             = -412
        val SignalInterfaceUnavailable         = -413
        val SignalInterfaceConstructorInvalid  = -414
        val SignalInterfaceDestructorInvalid   = -415
        val SignalInterfaceUpdateInvalid       = -416
        val SignalPortKeyInvalid               = -420
        val SignalPortUnknown                  = -421
        val SignalPortInvalid                  = -422
        val SignalPortUnavailable              = -423
        val SignalPortConstructorInvalid       = -424
        val SignalPortDestructorInvalid        = -425
        val SignalPortUpdateInvalid            = -426
        val SignalPortTapless                  = -427
        val SignalSourceKeyInvalid             = -430
        val SignalSourceUnknown                = -431
        val SignalSourceInvalid                = -432
        val SignalSourceUnavailable            = -433
        val SignalSourceConstructorInvalid     = -434
        val SignalSourceDestructorInvalid      = -435
        val SignalSourceUpdateInvalid          = -436
        val SignalSourceLinkless               = -437
        val SignalSinkKeyInvalid               = -440
        val SignalSinkUnknown                  = -441
        val SignalSinkInvalid                  = -442
        val SignalSinkUnavailable              = -443
        val SignalSinkConstructorInvalid       = -444
        val SignalSinkDestructorInvalid        = -445
        val SignalSinkUpdateInvalid            = -446
        val SignalSinkTapless                  = -447
        val SignalLinkKeyInvalid               = -450
        val SignalLinkUnknown                  = -451
        val SignalLinkInvalid                  = -452
        val SignalLinkUnavailable              = -453
        val SignalLinkConstructorInvalid       = -454
        val SignalLinkDestructorInvalid        = -455
        val SignalLinkUpdateInvalid            = -456
        val SignalLinkSinkless                 = -457
        val SignalTapKeyInvalid                = -460
        val SignalTapUnknown                   = -461
        val SignalTapInvalid                   = -462
        val SignalTapUnavailable               = -463
        val SignalTapConstructorInvalid        = -464
        val SignalTapDestructorInvalid         = -465
        val SignalTapUpdateInvalid             = -466
        val SignalTapSourceless                = -467
        val SignalTapSinkless                  = -468
        val SignalTapModulatorless             = -469
        val CellKeyInvalid                     = -900
        val CellUnknown                        = -901
        val CellInvalid                        = -902
        val CellUnavailable                    = -903
    }

    val kActorName = "Cell"

    trait CellMessage
        extends Message

    object RequestMessages
    {

        case object Start
            extends SupervisionMessage

        case object Stop
            extends SupervisionMessage

        case class ServiceCall (_serviceCall: org.taranos.common.ServiceCall)
            extends CellMessage

    }

    object ResponseMessages
    {

        case object Started
            extends SupervisionMessage

        case class ServiceResult (_serviceResult: org.taranos.common.ServiceResult)
            extends CellMessage

    }

    type UniqueKey = String

    class Key (uniqueKey: UniqueKey, symbol: Symbol = 'Cell)

    case class KillOrder (
        _trunkKey: Trunk.Key,
        _mark: Int)

    trait CellReporter
        extends Reporter
    {
        def Report (sectionsOpt: Option[String]): JsObject = Json.obj()
    }

    def MakeActorName (uniqueKey: UniqueKey) = "Cell" + ''' + uniqueKey

    def MakeActorProps (uniqueKey: UniqueKey) = akka.actor.Props(classOf[Cell], uniqueKey)

    def DebangTag (tag: String): (Boolean, String) =
    {
        if (tag.startsWith(Glossary.kBang))
            (true, tag.drop(1))
        else
            (false, tag)
    }

    def MakeUniqueKey (tag: String = "", isObscured: Boolean = true): UniqueKey =
    {
        val (wasBanged, debangedTag) = DebangTag(tag)

        if (!isObscured || wasBanged)
            debangedTag
        else
            java.util.UUID.randomUUID().toString
    }

    // String glossary:
    object Glossary
    {
        // punctuation
        val kDefaultPrefix = "_"
        val kBang = "!"

        // elements
        val kECell = "c"
        val kEDefaultCell = kDefaultPrefix + kECell
    }
}

trait CellLogger
{
    def LogDebug (text: String): Unit

    def LogInfo (text: String): Unit
}

class Cell (
    private val _uniqueKey: Cell.UniqueKey)
    extends akka.actor.Actor
        with CellLogger
        with CellReporter
{
    import scala.collection.mutable

    private
    val _log = akka.event.Logging(context.system, this)

    private implicit
    val _logger = this

    private implicit
    var _trunkModel: TrunkModel = null  // needs to be Reset()!

    private implicit
    var _fieldModel: FieldModel = null  // needs to be Reset()!

    private
    var _cycleOrdinal: Int = 0

    def GetCycleOrdinal = _cycleOrdinal

    private
    var _killOrders: mutable.ArrayBuffer[KillOrder] = null  // needs to be Reset()!

    def AddKillOrder (killOrder: KillOrder) = _killOrders += killOrder

    def Reset () =
    {
        // Allocate new trunk model:
        _trunkModel = new TrunkModel(this)

        // Allocate new field model associated with new trunk model:
        _fieldModel = new FieldModel(this, _trunkModel)
        _trunkModel.BindFieldModel(_fieldModel)

        // Reset the cycle ordinal:
        _cycleOrdinal = 0

        // Allocate new kill orders array:
        _killOrders = mutable.ArrayBuffer[KillOrder]()

        // Initialize the trunk model:
        _trunkModel.Initialize()

        // Initialize the field model:
        _fieldModel.Initialize()
    }

    private
    def HandleMessage_ServiceCall (serviceCall: ServiceCall): Unit =
    {
        def ReportException (what: String): String =
        {
            val excObject = Json.obj("e" -> what)
            Json.stringify(excObject)
        }

        val serviceName = serviceCall._name
        val serviceArgs = serviceCall._args

        val serviceResult = try
        {
            _cycleOrdinal = _cycleOrdinal + 1

            _killOrders.clear()

            val serviceResult = serviceName match
            {
                //
                // Rendering:
                //

                // Error code range 10x
                case "ReportFields" => RenderingServices.ReportFields(serviceArgs)
                case "UpdateFields" => RenderingServices.UpdateFields(serviceArgs)
                case "CreateFields" => RenderingServices.CreateFields(serviceArgs)
                case "DestroyFields" => RenderingServices.DestroyFields(serviceArgs)

                // Error code range 11x
                case "ReportFieldEmitters" => RenderingServices.ReportFieldEmitters(serviceArgs)
                case "UpdateFieldEmitters" => RenderingServices.UpdateFieldEmitters(serviceArgs)
                case "CallFieldEmitters" => RenderingServices.CallFieldEmitters(serviceArgs)
                case "CreateFieldEmitters" => RenderingServices.CreateFieldEmitters(serviceArgs)
                case "DestroyFieldEmitters" => RenderingServices.DestroyFieldEmitters(serviceArgs)

                // Error code range 12x
                case "ReportFieldOscillators" => RenderingServices.ReportFieldOscillators(serviceArgs)
                case "UpdateFieldOscillators" => RenderingServices.UpdateFieldOscillators(serviceArgs)
                case "CallFieldOscillators" => RenderingServices.CallFieldOscillators(serviceArgs)
                case "CreateFieldOscillators" => RenderingServices.CreateFieldOscillators(serviceArgs)
                case "DestroyFieldOscillators" => RenderingServices.DestroyFieldOscillators(serviceArgs)

                // Error code range 13x
                case "ReportSubjects" => RenderingServices.ReportSubjects(serviceArgs)
                case "UpdateSubjects" => RenderingServices.UpdateSubjects(serviceArgs)
                case "CreateSubjects" => RenderingServices.CreateSubjects(serviceArgs)
                case "DestroySubjects" => RenderingServices.DestroySubjects(serviceArgs)

                // Error code range 14x
                case "ReportSubjectEmitters" => RenderingServices.ReportSubjectEmitters(serviceArgs)
                case "UpdateSubjectEmitters" => RenderingServices.UpdateSubjectEmitters(serviceArgs)
                case "CallSubjectEmitters" => RenderingServices.CallSubjectEmitters(serviceArgs)
                case "CreateSubjectEmitters" => RenderingServices.CreateSubjectEmitters(serviceArgs)
                case "DestroySubjectEmitters" => RenderingServices.DestroySubjectEmitters(serviceArgs)

                // Error code range 15x
                case "ReportSubjectOscillators" => RenderingServices.ReportSubjectOscillators(serviceArgs)
                case "UpdateSubjectOscillators" => RenderingServices.UpdateSubjectOscillators(serviceArgs)
                case "CallSubjectOscillators" => RenderingServices.CallSubjectOscillators(serviceArgs)
                case "CreateSubjectOscillators" => RenderingServices.CreateSubjectOscillators(serviceArgs)
                case "DestroySubjectOscillators" => RenderingServices.DestroySubjectOscillators(serviceArgs)

                // Error code range 16x
                case "ReportProbes" => RenderingServices.ReportProbes(serviceArgs)
                case "UpdateProbes" => RenderingServices.UpdateProbes(serviceArgs)
                case "CreateProbes" => RenderingServices.CreateProbes(serviceArgs)
                case "DestroyProbes" => RenderingServices.DestroyProbes(serviceArgs)

                // Error code range 17x
                case "ReportProbeEmitters" => RenderingServices.ReportProbeEmitters(serviceArgs)
                case "UpdateProbeEmitters" => RenderingServices.UpdateProbeEmitters(serviceArgs)
                case "CallProbeEmitters" => RenderingServices.CallProbeEmitters(serviceArgs)
                case "CreateProbeEmitters" => RenderingServices.CreateProbeEmitters(serviceArgs)
                case "DestroyProbeEmitters" => RenderingServices.DestroyProbeEmitters(serviceArgs)

                // Error code range 18x
                case "ReportProbeOscillators" => RenderingServices.ReportProbeOscillators(serviceArgs)
                case "UpdateProbeOscillators" => RenderingServices.UpdateProbeOscillators(serviceArgs)
                case "CallProbeOscillators" => RenderingServices.CallProbeOscillators(serviceArgs)
                case "CreateProbeOscillators" => RenderingServices.CreateProbeOscillators(serviceArgs)
                case "DestroyProbeOscillators" => RenderingServices.DestroyProbeOscillators(serviceArgs)

                // Error code range 19x
                case "LookupProbeCollector" => RenderingServices.LookupProbeCollector(serviceArgs)
                case "ReportProbeCollectors" => RenderingServices.ReportProbeCollectors(serviceArgs)
                case "UpdateProbeCollectors" => RenderingServices.UpdateProbeCollectors(serviceArgs)
                case "CreateProbeCollectors" => RenderingServices.CreateProbeCollectors(serviceArgs)
                case "DestroyProbeCollectors" => RenderingServices.DestroyProbeCollectors(serviceArgs)

                // Error code range 20x
                case "ReportWaveformsAtProbeCollector" => RenderingServices.ReportWaveformsAtProbeCollector(serviceArgs)
                case "ReportWaveformsAtSampler" => RenderingServices.ReportWaveformsAtSampler(serviceArgs)

                // Error code range 30x
                case "ReportEmitterPatches" => RenderingServices.ReportEmitterPatches(serviceArgs)
                case "ReportPatchOfFieldEmitter" => RenderingServices.ReportPatchOfFieldEmitter(serviceArgs)
                case "ReportPatchOfSubjectEmitter" => RenderingServices.ReportPatchOfSubjectEmitter(serviceArgs)
                case "ReportPatchOfProbeEmitter" => RenderingServices.ReportPatchOfProbeEmitter(serviceArgs)
                case "UpdateEmitterPatches" => RenderingServices.UpdateEmitterPatches(serviceArgs)

                // Error code range 31x
                case "ReportOscillatorPatches" => RenderingServices.ReportOscillatorPatches(serviceArgs)
                case "ReportPatchOfFieldOscillator" => RenderingServices.ReportPatchOfFieldOscillator(serviceArgs)
                case "ReportPatchOfSubjectOscillator" => RenderingServices.ReportPatchOfSubjectOscillator(serviceArgs)
                case "ReportPatchOfProbeOscillator" => RenderingServices.ReportPatchOfProbeOscillator(serviceArgs)
                case "UpdateOscillatorPatches" => RenderingServices.UpdateOscillatorPatches(serviceArgs)

                // Error code range 32x
                case "ReportOscillatorPatchEnvelopes" => RenderingServices.ReportOscillatorPatchEnvelopes(serviceArgs)
                case "UpdateOscillatorPatchEnvelopes" => RenderingServices.UpdateOscillatorPatchEnvelopes(serviceArgs)

                //
                // Signaling:
                //

                // Error code range 33x
                case "ReportSignalInputs" => SignalingServices.ReportSignalInputs(serviceArgs)
                case "UpdateSignalInputs" => SignalingServices.UpdateSignalInputs(serviceArgs)
                case "CreateSignalInputs" => SignalingServices.CreateSignalInputs(serviceArgs)
                case "DestroySignalInputs" => SignalingServices.DestroySignalInputs(serviceArgs)

                // Error code range 34x
                case "ReportSignalBridges" => SignalingServices.ReportSignalBridges(serviceArgs)
                case "UpdateSignalBridges" => SignalingServices.UpdateSignalBridges(serviceArgs)
                case "CreateSignalBridges" => SignalingServices.CreateSignalBridges(serviceArgs)
                case "DestroySignalBridges" => SignalingServices.DestroySignalBridges(serviceArgs)

                // Error code range 35x
                case "ReportSignalOutputs" => SignalingServices.ReportSignalOutputs(serviceArgs)
                case "UpdateSignalOutputs" => SignalingServices.UpdateSignalOutputs(serviceArgs)
                case "CreateSignalOutputs" => SignalingServices.CreateSignalOutputs(serviceArgs)
                case "DestroySignalOutputs" => SignalingServices.DestroySignalOutputs(serviceArgs)

                // Error code range 40x
                case "ReportTrunks" => SignalingServices.ReportTrunks(serviceArgs)
                case "UpdateTrunks" => SignalingServices.UpdateTrunks(serviceArgs)
                case "CreateTrunks" =>  SignalingServices.CreateTrunks(serviceArgs)
                case "DestroyTrunks" => SignalingServices.DestroyTrunks(serviceArgs)

                // Error code range 41x
                case "ReportSignalInterfaces" => SignalingServices.ReportSignalInterfaces(serviceArgs)
                case "UpdateSignalInterfaces" => SignalingServices.UpdateSignalInterfaces(serviceArgs)
                case "CreateSignalInterfaces" => SignalingServices.CreateSignalInterfaces(serviceArgs)
                case "DestroySignalInterfaces" => SignalingServices.DestroySignalInterfaces(serviceArgs)

                // Error code range 42x
                case "LookupSignalPort" => SignalingServices.LookupSignalPort(serviceArgs)
                case "ReportSignalPorts" => SignalingServices.ReportSignalPorts(serviceArgs)
                case "UpdateSignalPorts" => SignalingServices.UpdateSignalPorts(serviceArgs)
                case "CreateSignalPorts" => SignalingServices.CreateSignalPorts(serviceArgs)
                case "DestroySignalPorts" => SignalingServices.DestroySignalPorts(serviceArgs)

                // Error code range 43x
                case "ReportSignalSources" => SignalingServices.ReportSignalSources(serviceArgs)
                case "UpdateSignalSources" => SignalingServices.UpdateSignalSources(serviceArgs)
                case "CreateSignalSources" => SignalingServices.CreateSignalSources(serviceArgs)
                case "DestroySignalSources" => SignalingServices.DestroySignalSources(serviceArgs)

                // Error code range 44x
                case "ReportSignalSinks" => SignalingServices.ReportSignalSinks(serviceArgs)
                case "UpdateSignalSinks" => SignalingServices.UpdateSignalSinks(serviceArgs)
                case "CreateSignalSinks" => SignalingServices.CreateSignalSinks(serviceArgs)
                case "DestroySignalSinks" => SignalingServices.DestroySignalSinks(serviceArgs)

                // Error code range 45x
                case "ReportSignalLinks" => SignalingServices.ReportSignalLinks(serviceArgs)
                case "UpdateSignalLinks" => SignalingServices.UpdateSignalLinks(serviceArgs)
                case "CreateSignalLinks" => SignalingServices.CreateSignalLinks(serviceArgs)
                case "DestroySignalLinks" => SignalingServices.DestroySignalLinks(serviceArgs)

                // Error code range 46x
                case "ReportSignalTaps" => SignalingServices.ReportSignalTaps(serviceArgs)
                case "UpdateSignalTaps" => SignalingServices.UpdateSignalTaps(serviceArgs)
                case "CreateSignalTaps" => SignalingServices.CreateSignalTaps(serviceArgs)
                case "DestroySignalTaps" => SignalingServices.DestroySignalTaps(serviceArgs)

                //
                // Cell:
                //

                // Error code range 50x
                case "DestroyCell" => ManagementServices.DestroyCell(serviceArgs)
                case "ReportCell" => ManagementServices.ReportCell(serviceArgs)

                //
                // Debug:
                //

                case "@ReportFieldModel" =>
                    // Decode field key:
                    val fieldKeyEncoded = serviceArgs.head
                    val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)

                    // Decode query:
                    val queryEncoded = serviceArgs(1)
                    val query = FieldModel.DecodeQuery(queryEncoded)

                    // Report field model:
                    val fieldModelReport = _fieldModel.Report(fieldKey, query._sectionsOpt)

                    // Wrap report:
                    val wrapper = Json.obj("rmf" -> fieldModelReport)

                    // Stringify wrapper:
                    val results = Vector(Json.stringify(wrapper))

                    // Reply with report:
                    ServiceResult(0, results)

                case "@ReportTrunkModel" =>
                    // Decode trunk key:
                    val trunkKeyEncoded = serviceArgs.head
                    val trunkKey = TrunkElement.DecodeKey[Trunk.Key](trunkKeyEncoded)

                    // Decode query:
                    val queryEncoded = serviceArgs(1)
                    val query = TrunkModel.DecodeQuery(queryEncoded)

                    // Report trunk model:
                    val trunkModelReport = _trunkModel.Report(trunkKey, query._sectionsOpt)

                    // Wrap report:
                    val wrapper = Json.obj("rmt" -> trunkModelReport)

                    // Stringify wrapper:
                    val results = Vector(Json.stringify(wrapper))

                    // Reply with report:
                    ServiceResult(0, results)

                case "@Field_DumpElementCounts" =>
                    // Decode field key:
                    val fieldKeyEncoded = serviceArgs.head
                    val fieldKey = FieldElement.DecodeKey[Field.Key](fieldKeyEncoded)

                    _fieldModel.DumpElementCounts(fieldKey)

                    // Reply with reports:
                    ServiceResult(0, Vector(""))

                case "@Trunk_DumpElementCounts" =>
                    // Decode trunk key:
                    val trunkKeyEncoded = serviceArgs.head
                    val trunkKey = TrunkElement.DecodeKey[Trunk.Key](trunkKeyEncoded)

                    _trunkModel.DumpElementCounts(trunkKey)

                    // Reply with reports:
                    ServiceResult(0, Vector(""))

                case _ =>
                    throw new Throwable("unknown service")
            }

            // Process kill orders:
            _killOrders.foreach(killOrder =>
            {
                // TBD: Avoid execution of duplicate orders
                _trunkModel.Kill(killOrder)
            })

            serviceResult
        }
        catch
        {
            case exc: FieldException =>
                ServiceResult(
                    exc._code,
                    Vector(ReportException("<<<FieldException>>>:(" + exc._code + ")[" + exc._text + "]")))

            case exc: TrunkException =>
                ServiceResult(
                    exc._code,
                    Vector(ReportException("<<<TrunkException>>>:(" + exc._code + ")[" + exc._text + "]")))

            case exc: java.lang.AssertionError =>
                exc.printStackTrace()
                ServiceResult(-1, Vector(ReportException(exc.toString)))

            case exc: Throwable =>
                exc.printStackTrace()
                ServiceResult(-1, Vector(ReportException("Cell service failed: " + exc)))
        }

        sender ! Cell.ResponseMessages.ServiceResult(serviceResult)
    }

    private
    def HandleMessage_Start (): Unit =
    {
        Reset()

        // Tell supervisor we've started:
        context.parent ! Cell.ResponseMessages.Started
    }

    def LogDebug (text: String): Unit = _log.debug(text)

    def LogInfo (text: String): Unit = _log.info(text)

    override
    def Report (sectionsOpt: Option[String] = None): JsObject =
    {
        var report = Json.obj()

//        val sections = new ReportSectionsParser(sectionsOpt)

        // Add model reports:
        report ++= Json.obj(
            "mf" -> _fieldModel.Report(sectionsOpt = sectionsOpt),
            "mt" -> _trunkModel.Report(sectionsOpt = sectionsOpt))

        report
    }

    //
    // akka.actor.Actor
    //

    override
    def postRestart (reason: Throwable): Unit =
    {
        super.postRestart(reason)
    }

    override
    def postStop (): Unit =
    {
        super.postStop()
    }

    override
    def preRestart (
        reason: Throwable,
        message: Option[Any]): Unit =
    {
        super.preRestart(reason, message)
    }

    override
    def preStart (): Unit =
    {
        super.preStart()
    }

    def receive =
    {
        //
        // From supervisor:
        //

        case Cell.RequestMessages.Start =>
            HandleMessage_Start()

        //
        // From supervised:
        //

        //
        // From clients:
        //

        case Cell.RequestMessages.ServiceCall (serviceCall) =>
            HandleMessage_ServiceCall(serviceCall)

        //
        // From self:
        //

        //
        // Other:
        //

        case unhandled =>
            _log.warning(s"received unhandled message (${unhandled.getClass})")
            assert(false)
    }
}
