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


class ProbeCollectorPlant
(implicit
    protected val _fieldModel: FieldModel,
    protected val _trunkModel: TrunkModel)
    extends FieldElementPlant
{
    import scala.collection.mutable

    private
    val _probeCollectors = mutable.HashMap.empty[(Field.Key, ProbeCollector.Key), ProbeCollector]

    def CreateProbeCollector (
        field: Field,
        probe: Probe,
        constructor: ProbeCollector.Constructor): ProbeCollector =
    {
        // 1: Create element:
        val collector = new ProbeCollector(
            new ProbeCollector.Meta(
                FieldElement.MakeUniqueKey(constructor._tag),
                constructor._tag,
                constructor._badgeOpt,
                constructor._nameOpt,
                constructor._descriptionOpt,
                constructor._aliasOpt),
            new ProbeCollector.Attrs(
                acoustic_a = constructor._acoustic_aOpt.getOrElse(Collector.Defaults.kAcoustic_a),
                lobeBearingPolesOpt = constructor._lobeBearingPolesOpt,
                lobeRangeOpt = constructor._lobeRangeOpt,
                lobeRangePolesOpt = constructor._lobeRangePolesOpt,
                squelchThresholdOpt = constructor._squelchThresholdOpt),
            new ProbeCollector.Refs(
                field.GetKey,
                probe.GetKey))

        // 2: Add element to store:
        _probeCollectors += (field.GetKey, collector.GetKey) -> collector

        // 3: Bind with/create children:
        // N/A

        // 4: Bind with peers:
        // N/A

        // 5: Bind with parent:
        probe.BindCollector(collector.GetKey)

        // 6: Activate element:
        // N/A

        // Return element:
        collector
    }

    def DestroyProbeCollector (
        field: Field,
        destructor: ProbeCollector.Destructor): ProbeCollector.Key =
    {
        _probeCollectors.get((field.GetKey, destructor._key)) match
        {
            case Some(collector) =>
                // 1: Unbind with parent:
                val probe = _fieldModel.GetProbeOpt(
                    field.GetKey,
                    collector.GetParentKey.asInstanceOf[Probe.Key]).get
                val isUnbound = probe.UnbindCollector(collector.GetKey, isForcedDestroy = true)
                if (isUnbound)
                {
                    // 2: Deactivate element:
                    // N/A

                    // 3: Unbind with peers:
                    // N/A

                    // 4: Unbind with/destroy children:
                    // N/A

                    // 5: Remove element from store:
                    _probeCollectors -= ((field.GetKey, collector.GetKey))
                }

            case None => throw new FieldException(Cell.ErrorCodes.ProbeCollectorUnknown)
        }

        // Return probe collector key:
        destructor._key
    }

    def DestroyAllProbeCollectors (
        field: Field,
        scope: Symbol): Unit =
    {
        val fieldKey = field.GetKey

        // Destroy each probe collector of {field, probe}:
        _probeCollectors.filter(_._1._1 == fieldKey).foreach(collectorPair =>
        {
            val ((_, collectorKey), _) = collectorPair
            val probeCollectorDestructor = new ProbeCollector.Destructor(collectorKey, scope)
            DestroyProbeCollector(field, probeCollectorDestructor)
        })
    }

    def GetProbeCollectorOpt (
        field: Field,
        key: ProbeCollector.Key,
        isRequired: Boolean = true): Option[ProbeCollector] =
    {
        // Lookup probe collector:
        key match
        {
            case _: ProbeCollector.Key =>
                val opt = _probeCollectors.get((field.GetKey, key))
                if (isRequired && opt.isEmpty)
                    throw new FieldException(Cell.ErrorCodes.ProbeCollectorUnknown)
                opt

            case _ => throw new FieldException(Cell.ErrorCodes.ProbeCollectorKeyInvalid)
        }
    }

    def GetProbeCollectors (
        field: Field,
        probeOpt: Option[Probe] = None): Vector[ProbeCollector] =
    {
        // Return probe collectors vector:
        val fieldKey = field.GetKey
        if (probeOpt.isDefined)
        {
            val probeKey = probeOpt.get.GetKey
            _probeCollectors.filter(pair =>
            {
                val ((pairFieldKey, pairCollectorKey), _) = pair
                pairFieldKey == fieldKey &&
                    _fieldModel.GetProbeCollectorOpt(fieldKey, pairCollectorKey).get.GetParentKey == probeKey
            }).values.toVector
        }
        else
            _probeCollectors.filter(pair =>
            {
                val ((pairFieldKey, _), _) = pair
                pairFieldKey == fieldKey
            }).values.toVector
    }

    def GetProbeCollectorKeys (
        field: Field,
        probeOpt: Option[Probe] = None): Vector[ProbeCollector.Key] =
    {
        // Return probe collector keys vector:
        val fieldKey = field.GetKey
        if (probeOpt.isDefined)
        {
            val probeKey = probeOpt.get.GetKey
            _probeCollectors.filter(pair =>
            {
                val ((pairFieldKey, pairCollectorKey), _) = pair
                pairFieldKey == fieldKey &&
                    _fieldModel.GetProbeCollectorOpt(fieldKey, pairCollectorKey).get.GetParentKey == probeKey
            }).keys.map(pair => pair._2).toVector
        }
        else
            _probeCollectors.filter(pair =>
            {
                val ((pairFieldKey, _), _) = pair
                pairFieldKey == fieldKey
            }).keys.map(pair => pair._2).toVector
    }

    def GetElementCount (fieldKey: Field.Key): Int = _probeCollectors.count(_._1._1 == fieldKey)

    def LookupProbeCollector (field: Field, lookupAlias: String): Option[ProbeCollector.Key] =
    {
        import scala.util.control.Breaks._

        val fieldKey = field.GetKey

        var probeCollectorOpt: Option[ProbeCollector.Key] = None
        breakable
        {
            for (portPair <- _probeCollectors.filter(_._1._1 == fieldKey))
            {
                val ((_, pairCollectorKey), pairCollector) = portPair
                pairCollector.GetAliasOpt match
                {
                    case Some(alias) =>
                        if (alias == lookupAlias)
                        {
                            probeCollectorOpt = Some(pairCollectorKey)
                            break()
                        }

                    case None =>
                }
            }
        }

        probeCollectorOpt
    }
}
