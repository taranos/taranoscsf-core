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


class SignalTapPlant
    (implicit protected val _trunkModel: TrunkModel)
    extends TrunkElementPlant
{
    import scala.collection.mutable

    private
    val _taps = mutable.HashMap.empty[(Trunk.Key, SignalTap.Key), SignalTap]

    def CreateSignalTap (
        trunk: Trunk,
        constructor: SignalTap.Constructor): SignalTap =
    {
        // Create sink element (child):
        val sinkConstructor = new SignalSink.Constructor(
            _tag = constructor._tag + TrunkModel.Glossary.kTagSeparator + TrunkModel.Glossary.kESignalSink,
            _mode = constructor._mode)
        val sink = _trunkModel.CreateSignalSinks(trunk.GetKey, Vector(sinkConstructor)).head

        // Create source element (child):
        val sourceConstructor = new SignalSource.Constructor(
            _tag = constructor._tag + TrunkModel.Glossary.kTagSeparator + TrunkModel.Glossary.kESignalSource)
        val source = _trunkModel.CreateSignalSources(trunk.GetKey, Vector(sourceConstructor)).head

        // Create tap element:
        val tap = new SignalTap(
            new SignalTap.Meta(
                TrunkElement.MakeUniqueKey(constructor._tag),
                constructor._tag,
                constructor._badgeOpt,
                constructor._nameOpt,
                constructor._descriptionOpt,
                constructor._mode),
            new SignalTap.Attrs(),
            new SignalTap.Refs(
                trunk.GetKey,
                sink.GetKey,
                source.GetKey,
                constructor._modulatorKeyOpt))

        // 1: Add element to store:
        _taps += (trunk.GetKey, tap.GetKey) -> tap

        // 2: Bind with trunk:
        trunk.BindSignalTap(tap.GetKey)

        // 3: Bind with parent:
        // N/A
        
        // 4: Bind with peers:
        // N/A

        // 5: Bind with children:
        // N/A

        // Set tap to be sink listener by default:
        sink.SetListenerOpt(Some(tap))

        // Return tap:
        tap
    }

    def DestroySignalTap (
        trunk: Trunk,
        destructor: SignalTap.Destructor): SignalTap.Key =
    {
        destructor._key match
        {
            case key: SignalTap.Key =>
                _taps.get((trunk.GetKey, key)) match
                {
                    case Some(tap) =>
                        // 1: Unbind with children:
                        val sinkKey = tap.GetSinkKey
                        val sourceKey = tap.GetSourceKey
                        tap.UnbindAll(isReciprocal = true)

                        // 2: Unbind with peers:
                        // N/A

                        // 3: Unbind with parent:
                        // N/A

                        // 4: Unbind with trunk:
                        trunk.UnbindSignalTap(tap.GetKey)

                        // 5: Destroy children:
                        // Sink:
                        if (sinkKey != SignalSink.kNoneKey)
                        {
                            val sinkDestructor = new SignalSink.Destructor(sinkKey)
                            _trunkModel.DestroySignalSinks(trunk.GetKey, Vector(sinkDestructor))
                        }
                        // Source:
                        if (sourceKey != SignalSource.kNoneKey)
                        {
                            val sourceDestructor = new SignalSource.Destructor(sourceKey)
                            _trunkModel.DestroySignalSources(trunk.GetKey, Vector(sourceDestructor))
                        }

                        // 3: Remove element from store:
                        _taps -= ((trunk.GetKey, key))

                    case None => throw new TrunkException(Cell.ErrorCodes.SignalTapUnknown)
                }

            case _ => throw new TrunkException(Cell.ErrorCodes.SignalTapInvalid)
        }

        // Return tap key:
        destructor._key
    }

    def DestroyAllSignalTaps (trunk: Trunk): Unit =
    {
        val trunkKey = trunk.GetKey

        // Destroy each tap of trunk:
        _taps.filter(_._1._1 == trunkKey).foreach(tapPair =>
        {
            val ((_, pairTapKey), _) = tapPair
            val tapDestructor = new SignalTap.Destructor(pairTapKey)
            DestroySignalTap(trunk, tapDestructor)
        })
    }

    def GetSignalTapOpt (
        trunk: Trunk,
        key: SignalTap.Key,
        isRequired: Boolean = true): Option[SignalTap] =
    {
        // Lookup tap key:
        key match
        {
            case _: SignalTap.Key =>
                val opt = _taps.get((trunk.GetKey, key))
                if (isRequired && opt.isEmpty)
                    throw new TrunkException(Cell.ErrorCodes.SignalTapUnknown)
                opt

            case _ => throw new TrunkException(Cell.ErrorCodes.SignalTapKeyInvalid)
        }
    }

    def GetSignalTaps (trunk: Trunk): Vector[SignalTap] =
    {
        val trunkKey = trunk.GetKey

        // Return taps vector:
        _taps.filter(_._1._1 == trunkKey).values.toVector
    }

    def GetSignalTapKeys (trunk: Trunk): Vector[SignalTap.Key] =
    {
        val trunkKey = trunk.GetKey

        // Return tap keys vector:
        _taps.filter(_._1._1 == trunkKey).keys.map(_._2).toVector
    }

    def GetElementCount (trunkKey: Trunk.Key): Int = _taps.count(_._1._1 == trunkKey)
}
