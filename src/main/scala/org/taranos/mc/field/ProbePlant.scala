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

package org.taranos.mc.field

import org.taranos.mc.Cell
import org.taranos.mc.trunk.intraprocess.TrunkModel


class ProbePlant
    (implicit
        protected val _fieldModel: FieldModel,
        protected val _trunkModel: TrunkModel)
    extends FieldElementPlant
{
    import scala.collection.mutable

    private
    val _probes = mutable.HashMap.empty[(Field.Key, Probe.Key), Probe]

    def CreateProbe (
        field: Field,
        constructor: Probe.Constructor): Probe =
    {
        // 1: Create element:
        val probe = new Probe(
            new Probe.Meta(
                FieldElement.MakeUniqueKey(constructor._tag),
                constructor._tag,
                constructor._badgeOpt,
                constructor._nameOpt,
                constructor._descriptionOpt),
            new Probe.Attrs(),
            new Probe.Refs(field.GetKey),
            new Probe.State(
                constructor._position,
                constructor._rotation))

        // 2: Add element to store:
        _probes += (field.GetKey, probe.GetKey) -> probe

        // 3: Bind with/create children:
        // Create default collector:
        val tag = constructor._tag + FieldModel.Glossary.kTagSeparator + FieldModel.Glossary.kEDefaultCollector
        val (_, debangedAlias) = FieldElement.DebangTag(tag)
        val collectorConstructor = ProbeCollector.Constructor(
            _tag = tag,
            _aliasOpt = Some(debangedAlias),
            _acoustic_aOpt = constructor._acoustic_aOpt,
            _squelchThresholdOpt = constructor._squelchThresholdOpt,
            _lobeRangeOpt = constructor._lobeRangeOpt,
            _lobeRangePolesOpt = constructor._lobeRangePolesOpt,
            _lobeBearingPolesOpt = constructor._lobeBearingPolesOpt)
        _fieldModel.CreateProbeCollectors(
            field.GetKey,
            probe.GetKey,
            Vector(collectorConstructor)).head

        // 4: Bind with peers:
        // N/A

        // 5: Bind with parent:
        field.BindProbe(probe.GetKey)

        // 6: Activate element:
        // N/A

        // Return element:
        probe
    }

    def DestroyProbe (
        field: Field,
        destructor: Probe.Destructor): Probe.Key =
    {
        _probes.get((field.GetKey, destructor._key)) match
        {
            case Some(probe) =>
                // 1: Unbind with parent:
                field.UnbindProbe(destructor._key)

                // 2: Deactivate element:
                // N/A

                // 3: Unbind with peers:
                // N/A

                // 4: Unbind with/destroy children:
                // Unbind collectors:
                val collectorDestructors =
                    probe.GetCollectorKeys.map(ProbeCollector.Destructor(_, destructor._scope))
                if (collectorDestructors.nonEmpty)
                    _fieldModel.DestroyProbeCollectors(field.GetKey, collectorDestructors.toVector)
                // Unbind emitters:
                val emitterDestructors =
                    probe.GetEmitterKeys.asInstanceOf[Set[ProbeEmitter.Key]].map(
                        ProbeEmitter.Destructor(_, destructor._scope))
                if (emitterDestructors.nonEmpty)
                    _fieldModel.DestroyProbeEmitters(field.GetKey, emitterDestructors.toVector)

                // 5: Remove element from store:
                _probes -= ((field.GetKey, destructor._key))

            case None => throw FieldException(Cell.ErrorCodes.ProbeUnknown)
        }

        // Return probe key:
        destructor._key
    }

    def DestroyAllProbes (
        field: Field,
        scope: Symbol): Unit =
    {
        val fieldKey = field.GetKey

        _probes.filter(_._1._1 == fieldKey).foreach(probePair =>
        {
            val ((_, pairProbeKey), _) = probePair
            val probeDestructor = Probe.Destructor(pairProbeKey, scope)
            DestroyProbe(field, probeDestructor)
        })
    }

    def GetProbeOpt (
        field: Field,
        key: Probe.Key,
        isRequired: Boolean = true): Option[Probe] =
    {
        // Lookup probe:
        key match
        {
            case _: Probe.Key =>
                val opt = _probes.get((field.GetKey, key))
                if (isRequired && opt.isEmpty)
                    throw FieldException(Cell.ErrorCodes.ProbeUnknown)
                opt

            case _ => throw FieldException(Cell.ErrorCodes.ProbeKeyInvalid)
        }
    }

    def GetProbes (field: Field): Vector[Probe] =
    {
        val fieldKey = field.GetKey

        // Return probes vector:
        _probes.filter(_._1._1 == fieldKey).values.toVector
    }

    def GetElementCount (fieldKey: Field.Key): Int =
        _probes.size
}
