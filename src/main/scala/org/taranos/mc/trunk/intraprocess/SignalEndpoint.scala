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

import org.taranos.mc.Common.ReportSectionsParser
import play.api.libs.json.{JsObject, Json}


object SignalEndpoint
{
    import scala.collection.mutable

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
        val _linkKeys: mutable.HashMap[String, SignalLink.Key] = mutable.HashMap.empty[String, SignalLink.Key],
        var _tapKeyOpt: Option[SignalTap.Key] = None)
        extends TrunkElement.Refs(trunkKey)
    {
        override
        def Report (sections: ReportSectionsParser): JsObject =
        {
            var report = super.Report(sections)

            if (!sections.HasPeerReports)
            {
                if (_linkKeys.nonEmpty)
                    report ++=
                        Json.obj(TrunkModel.Glossary.kESignalLink ->
                            _linkKeys.map(pair =>
                            {
                                val (part, linkKey) = pair
                                Json.obj(part -> TrunkElement.EncodeKey(linkKey))
                            }))

                if (_tapKeyOpt.isDefined)
                    report ++=
                        Json.obj(TrunkModel.Glossary.kESignalTap -> TrunkElement.EncodeKey(_tapKeyOpt.get))
            }

            report
        }
    }

    abstract
    class State
        extends TrunkElement.State
}

trait SignalEndpoint[KeyType <: SignalEndpoint.Key]
    extends BiasedElement[KeyType]
        with PropagatingElement
        with TestableElement
{
    /**
     * Associate the signal endpoint with a signal link.
     * @param linkKey Key of link to associate with.
     * @param part Name of part to associate with.
     * @param isReciprocal Association is reciprocal.
     */
    def BindLink (
        linkKey: SignalLink.Key,
        part: String,
        isReciprocal: Boolean): Unit

    /**
     * Associate the signal endpoint with a signal tap.
     * @param tapKey Key of tap to associate with.
     * @param isReciprocal Association is reciprocal.
     */
    def BindTap (
        tapKey: SignalTap.Key,
        isReciprocal: Boolean): Unit

    /**
     * Disassociate a signal endpoint from a signal link.
     * @param linkKeyOpt Key of the signal link to disassociate. Not all signal endpoint types support multiple links.
     *                   In that case no key is specified and the implied link is disassociated.
     * @param isReciprocal Disassociation is reciprocal.
     */
    def UnbindLink (
        linkKeyOpt: Option[SignalLink.Key] = None,
        isReciprocal: Boolean): Unit

    /**
     * Disassociate a signal endpoint from a signal tap.
     * @param isReciprocal Disassociation is reciprocal.
     */
    def UnbindTap (isReciprocal: Boolean): Unit
}

//case class SignalEndpointPair (
//    _upstreamEndpointKey: SignalSink.Key,
//    _downstreamEndpointKey: SignalSource.Key)
