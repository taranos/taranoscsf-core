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

import org.taranos.mc.Common.ReportSectionsParser
import play.api.libs.json.{JsObject, Json}


object SignalPath
{
    abstract
    class Key (uniqueKey: TrunkElement.UniqueKey, symbol: Symbol)
        extends BiasedElement.Key(uniqueKey, symbol)

    abstract
    class Meta[KeyType <: Key] (
        key: KeyType,
        tag: String,
        badgeOpt: Option[String],
        nameOpt: Option[String],
        descriptionOpt: Option[String],
        mode: Signal.ModeEnum.Mode)
        extends BiasedElement.Meta[KeyType](
            key,
            tag,
            badgeOpt,
            nameOpt,
            descriptionOpt,
            mode)

    abstract
    class Attrs
        extends TrunkElement.Attrs

    abstract
    class Refs (
        trunkKey: Trunk.Key,
        var _sinkKey: SignalSink.Key,
        var _sourceKey: SignalSource.Key,
        val _isTap: Boolean)
        extends TrunkElement.Refs(trunkKey)
    {
        override
        def Report (sections: ReportSectionsParser): JsObject =
        {
            var report = super.Report(sections)

            if (!sections.HasPeerReports)
            {
                if (_sinkKey != SignalSink.kNoneKey)
                    report ++=
                        Json.obj(TrunkModel.Glossary.kESignalSink -> TrunkElement.EncodeKey(_sinkKey))

                if (_sourceKey != SignalSource.kNoneKey)
                    report ++=
                        Json.obj(TrunkModel.Glossary.kESignalSource -> TrunkElement.EncodeKey(_sourceKey))
            }

            report
        }
    }

    abstract
    class State
        extends TrunkElement.State
}

trait SignalPath[KeyType <: SignalPath.Key]
    extends BiasedElement[KeyType]
        with PropagatorElement
        with TestableElement
{
    /**
     * Associate the signal path with a signal sink.
     * @param sinkKey Key of the signal sink to associate with.
     * @param isReciprocal Association is reciprocal.
     */
    def BindSink (
        sinkKey: SignalSink.Key,
        isReciprocal: Boolean): Unit

    /**
     * Associate the signal path with a signal source.
     * @param sourceKey Key of the signal source to associate with.
     * @param isReciprocal Association is reciprocal.
     */
    def BindSource (
        sourceKey: SignalSource.Key,
        isReciprocal: Boolean): Unit

    /**
     * Return the signal path's currently associated signal sink.
     */
    def GetSinkKey: SignalSink.Key

    /**
     * Return the signal path's currently associated signal source.
     */
    def GetSourceKey: SignalSource.Key

    /**
     * Disassociate all of the signal path's currently associated signal endpoints.
     * @param isReciprocal Disassociation is reciprocal.
     */
    def UnbindAll (isReciprocal: Boolean): Unit =
    {
        UnbindSink(isReciprocal)
        UnbindSource(isReciprocal)
    }

    /**
     * Disassociate the signal path's currently associated signal sink.
     * @param isReciprocal Disassociation is reciprocal.
     */
    def UnbindSink (isReciprocal: Boolean): Unit

    /**
     * Disassociate the signal path's currently associated signal source.
     * @param isReciprocal Disassociation is reciprocal.
     */
    def UnbindSource (isReciprocal: Boolean): Unit
}
