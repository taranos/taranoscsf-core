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

package org.taranos.mc.field

import org.taranos.mc.trunk.intraprocess._
import org.taranos.mc.{Cell, CellLogger}
import play.api.libs.json.{JsObject, Json}


class SubjectEmitterPlant
(implicit
    protected val _fieldModel: FieldModel,
    protected val _trunkModel: TrunkModel,
    protected val _logger: CellLogger)
    extends FieldElementPlant
{
    import scala.collection.mutable

    private
    val _subjectEmitters = mutable.HashMap.empty[(Field.Key, SubjectEmitter.Key), SubjectEmitter]

    private
    def CreateEmitterPatch (
        field: Field,
        emitterKey: Emitter.Key,
        tag: String,
        modulatorKey: SignalModulator.Key,
        patchDef: JsObject): EmitterPatch =
    {
        _trunkModel.CreateEmitterPatches(
            field.GetTrunkKey,
            Vector(
                new EmitterPatch.Constructor(
                    _tag = tag,
                    _fieldKey = field.GetKey,
                    _emitterKey = emitterKey,
                    _modulatableKey = modulatorKey,
                    _tappableKeyOpt = None,
                    _patchDef = patchDef))).head
    }

    private
    def DestroyEmitterPatch (
        field: Field,
        emitter: SubjectEmitter) =
    {
        _trunkModel.DestroyEmitterPatches(
            field.GetTrunkKey,
            Vector(new EmitterPatch.Destructor(emitter.GetPatchKey)))
    }

    def DeployEmitterPatch (
        field: Field,
        emitter: SubjectEmitter,
        patchDefOpt: Option[JsObject] = None): Unit =
    {
        val trunkKey: Trunk.Key = field.GetTrunkKey

        // Get emitter's patch:
        val emitterPatch =
            _trunkModel.GetEmitterPatchOpt(trunkKey, emitter.GetPatchKey).getOrElse(
                throw new FieldException(Cell.ErrorCodes.EmitterPatchUnknown))

        // Tell patch to import new patch definition:
        _logger.LogDebug(s"DEP: importing emitter patch definition: $patchDefOpt")
        emitterPatch.ImportEmitterPatchDef(patchDefOpt)

        // Destroy current oscillator elements (children):
        UndeployEmitterPatch(field, emitter)

        // Create new oscillator elements (children):
        emitterPatch.GetChannelsMap.foreach(pair =>
        {
            val (channelTag, channel) = pair

            val channelDef = Json.obj(
                FieldModel.Glossary.kChannelTag -> channelTag,
                FieldModel.Glossary.kOscillatorPatchDef -> channel._oscillatorPatchDef)
            val oscillatorConstructor = new SubjectOscillator.Constructor(
                _tag = emitter.GetTag + FieldModel.Glossary.kTagSeparator + channelTag,
                _channelDef = channelDef)
            _logger.LogDebug(s"DEP: creating SubjectOscillator: $oscillatorConstructor")
            _fieldModel.CreateSubjectOscillators(
                field.GetKey,
                emitter.GetKey,
                Vector(oscillatorConstructor))
        })
    }

    private
    def UndeployEmitterPatch (
        field: Field,
        emitter: SubjectEmitter) =
    {
        val trunkKey: Trunk.Key = field.GetTrunkKey

        // Get emitter's patch:
        val emitterPatch =
            _trunkModel.GetEmitterPatchOpt(trunkKey, emitter.GetPatchKey).getOrElse(
                throw new FieldException(Cell.ErrorCodes.EmitterPatchUnknown))

        emitterPatch.GetOscillatorPatchKeysMap.foreach(pair =>
        {
            val oscillatorPatchKey = pair._2
        
            val oscillatorPatch =
                _trunkModel.GetOscillatorPatchOpt(trunkKey, oscillatorPatchKey).getOrElse(
                    throw new FieldException(Cell.ErrorCodes.OscillatorPatchUnknown))
            val oscillatorKey = oscillatorPatch.GetOscillatorKey
        
            val oscillatorDestructor = new SubjectOscillator.Destructor(
                _key = oscillatorKey.asInstanceOf[SubjectOscillator.Key],
                _scope = 'ScopeDeep)
            _logger.LogDebug(s"DEP: destroying SubjectOscillator: $oscillatorDestructor")
            _fieldModel.DestroySubjectOscillators(
                fieldKey = field.GetKey,
                destructors = Vector(oscillatorDestructor),
                isForcedDestroy = true)
        })
        emitterPatch.ClearOscillatorPatchKeysMap()
    }

    def CreateSubjectEmitter (
        field: Field,
        subject: Subject,
        constructor: SubjectEmitter.Constructor): SubjectEmitter =
    {
        val trunkKey = field.GetTrunkKey

        // 1: Create element:
        val emitter = new SubjectEmitter(
            new SubjectEmitter.Meta(
                FieldElement.MakeUniqueKey(constructor._tag),
                constructor._tag,
                constructor._badgeOpt,
                constructor._nameOpt,
                constructor._descriptionOpt),
            new SubjectEmitter.Refs(
                field.GetKey,
                subject.GetKey))

        // 2: Add element to store:
        _subjectEmitters += (field.GetKey, emitter.GetKey) -> emitter

        // 3: Bind with/create children:
        {
            // Get modulator key, creating a new input if necessary:
            val modulatorKey: SignalModulator.Key = constructor._modulatorKeyOpt match
            {
                case Some(key) => key

                case None =>
                    // Create a new port:
                    val (_, debangedAlias) = FieldElement.DebangTag(constructor._tag)
                    val port = _trunkModel.CreateSignalPorts(
                        trunkKey,
                        SignalInterface.kAnyKey,
                        Vector(
                            new SignalPort.Constructor(
                                _tag = constructor._tag + FieldModel.Glossary.kTagSeparator + TrunkModel.Glossary.kESignalPort,
                                _aliasOpt = Some(debangedAlias),
                                _mode = Signal.ModeEnum.Continuous))).head

                    // Create input using new port's tap:
                    val input = _trunkModel.CreateSignalInputs(
                        trunkKey,
                        Vector(
                            new SignalInput.Constructor(
                                _tag = constructor._tag + FieldModel.Glossary.kTagSeparator + TrunkModel.Glossary.kESignalInput,
                                _mode = Signal.ModeEnum.Continuous,
                                _tappableKeyOpt = Some(port.GetTapKey)))).head
                    input.GetKey
            }

            // Get emitter patch definition, using default patch if necessary:
            val patchDef: JsObject = constructor._patchDefOpt.getOrElse(Patch.kDefaultEmitterPatchDef)

            // Create emitter patch element (child):
            CreateEmitterPatch(
                field = field,
                emitterKey = emitter.GetKey,
                tag = constructor._tag + FieldModel.Glossary.kTagSeparator + TrunkModel.Glossary.kEEmitterPatch,
                modulatorKey = modulatorKey,
                patchDef = patchDef)
        }

        // 4: Bind with peers:
        // N/A

        // 5: Bind with parent:
        subject.BindEmitter(emitter.GetKey)

        // 6: Activate element:
        // Deploy emitter patch:
        DeployEmitterPatch(field, emitter)

        // Return element:
        emitter
    }

    def DestroySubjectEmitter (
        field: Field,
        destructor: SubjectEmitter.Destructor,
        isForcedDestroy: Boolean): SubjectEmitter.Key =
    {
        _subjectEmitters.get((field.GetKey, destructor._key)) match
        {
            case Some(emitter) =>
                // 1: Unbind with parent:
                val subject = _fieldModel.GetSubjectOpt(
                    field.GetKey,
                    emitter.GetParentKey.asInstanceOf[Subject.Key]).get
                val isUnbound = subject.UnbindEmitter(emitter.GetKey, isForcedDestroy)
                if (isUnbound)
                {
                    // 2: Deactivate element:
                    UndeployEmitterPatch(field, emitter)

                    // 3: Unbind with peers:
                    // N/A

                    // 4: Unbind with/destroy children:
                    if (destructor._scope == 'ScopeDeep)
                        DestroyEmitterPatch(field, emitter)

                    // 5: Remove element from store:
                    _subjectEmitters -= ((field.GetKey, emitter.GetKey))
                }

            case None => throw new FieldException(Cell.ErrorCodes.SubjectEmitterUnknown)
        }

        // Return subject emitter key:
        destructor._key
    }

    def DestroyAllSubjectEmitters (
        field: Field,
        scope: Symbol): Unit =
    {
        val fieldKey = field.GetKey

        // Destroy each subject emitter of {field, subject}:
        _subjectEmitters.filter(_._1._1 == fieldKey).foreach(emitterPair =>
        {
            val ((_, pairEmitterKey), _) = emitterPair
            val subjectEmitterDestructor = new SubjectEmitter.Destructor(pairEmitterKey, scope)
            DestroySubjectEmitter(field, subjectEmitterDestructor, isForcedDestroy = true)
        })
    }

    def GetSubjectEmitterOpt (
        field: Field,
        key: SubjectEmitter.Key,
        isRequired: Boolean = true): Option[SubjectEmitter] =
    {
        // Lookup subject emitter:
        key match
        {
            case _: SubjectEmitter.Key =>
                val opt = _subjectEmitters.get((field.GetKey, key))
                if (isRequired && opt.isEmpty)
                    throw new FieldException(Cell.ErrorCodes.SubjectEmitterUnknown)
                opt

            case _ => throw new FieldException(Cell.ErrorCodes.SubjectEmitterKeyInvalid)
        }
    }

    def GetSubjectEmitters (
        field: Field,
        subjectOpt: Option[Subject] = None): Vector[SubjectEmitter] =
    {
        // Return subject emitters vector:
        val fieldKey = field.GetKey
        if (subjectOpt.isDefined)
        {
            val subjectKey = subjectOpt.get.GetKey
            _subjectEmitters.filter(pair =>
            {
                val ((pairFieldKey, pairEmitterKey), _) = pair
                pairFieldKey == fieldKey &&
                    _fieldModel.GetSubjectEmitterOpt(fieldKey, pairEmitterKey).get.GetParentKey == subjectKey
            }).values.toVector
        }
        else
            _subjectEmitters.filter(pair =>
            {
                val ((pairFieldKey, _), _) = pair
                pairFieldKey == fieldKey
            }).values.toVector
    }

    def GetSubjectEmitterKeys (
        field: Field,
        subjectOpt: Option[Subject] = None): Vector[SubjectEmitter.Key] =
    {
        // Return subject emitter keys vector:
        val fieldKey = field.GetKey
        if (subjectOpt.isDefined)
        {
            val subjectKey = subjectOpt.get.GetKey
            _subjectEmitters.filter(pair =>
            {
                val ((pairFieldKey, pairEmitterKey), _) = pair
                pairFieldKey == fieldKey &&
                    _fieldModel.GetSubjectEmitterOpt(fieldKey, pairEmitterKey).get.GetParentKey == subjectKey
            }).keys.map(pair => pair._2).toVector
        }
        else
            _subjectEmitters.filter(pair =>
            {
                val ((pairFieldKey, _), _) = pair
                pairFieldKey == fieldKey
            }).keys.map(pair => pair._2).toVector
    }

    def GetElementCount (fieldKey: Field.Key): Int = _subjectEmitters.count(_._1._1 == fieldKey)
}
