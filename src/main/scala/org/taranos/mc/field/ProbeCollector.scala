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

import org.taranos.mc.Cell
import org.taranos.mc.Common.{Real, ReportSectionsParser}
import org.taranos.mc.field.AliasedElement.AliasedConstructorMetaDecoder
import org.taranos.mc.field.FieldElement._
import org.taranos.mc.trunk.intraprocess.TrunkModel
import play.api.libs.json.{JsError, JsObject, JsSuccess, Json}


object ProbeCollector
{
    class Key (uniqueKey: FieldElement.UniqueKey, symbol: Symbol = 'ProbeCollector)
        extends Collector.Key(uniqueKey, symbol)

    class Meta (
        uniqueKey: FieldElement.UniqueKey,
        tag: String,
        badgeOpt: Option[String],
        nameOpt: Option[String],
        descriptionOpt: Option[String],
        var _aliasOpt: Option[String])
        extends Collector.Meta[ProbeCollector.Key](
            new ProbeCollector.Key(uniqueKey),
            tag,
            badgeOpt,
            nameOpt,
            descriptionOpt)
    {
        override
        def Report (sections: ReportSectionsParser): JsObject =
        {
            var report = super.Report(sections)

            if (_aliasOpt.isDefined)
                report ++=
                    Json.obj(FieldModel.Glossary.kPSMeta -> _aliasOpt.get)

            report
        }
    }

    class Attrs (
        acoustic_a: Real,
        lobeBearingPolesOpt: Option[String] = None,
        lobeRangeOpt: Option[Real] = None,
        lobeRangePolesOpt: Option[String] = None,
        squelchThresholdOpt: Option[Real] = None)
        extends Collector.Attrs(
            acoustic_a,
            lobeBearingPolesOpt,
            lobeRangeOpt,
            lobeRangePolesOpt,
            squelchThresholdOpt)

    class Refs (
        fieldKey: Field.Key,
        probeKey: Probe.Key)
        extends Collector.Refs(
            fieldKey,
            probeKey)

    class State
        extends Collector.State

    case class Constructor (
        _tag: String,
        _badgeOpt: Option[String] = None,
        _nameOpt: Option[String] = None,
        _descriptionOpt: Option[String] = None,
        _aliasOpt: Option[String],
        _acoustic_aOpt: Option[Real] = None,
        _squelchThresholdOpt: Option[Real] = None,
        _lobeRangeOpt: Option[Real] = None,
        _lobeRangePolesOpt: Option[String] = None,
        _lobeBearingPolesOpt: Option[String] = None)

    case class Destructor (
        _key: ProbeCollector.Key,
        _scope: Symbol)

    case class Query (
        keys: Vector[ProbeCollector.Key],
        sectionsOpt: Option[String] = None)
        extends FieldElement.Query[ProbeCollector.Key](
            keys,
            sectionsOpt)

    case class Update (
        _key: ProbeCollector.Key,
        _nameOpt: Option[String],
        _descriptionOpt: Option[String],
        _acoustic_aOpt: Option[Real],
        _squelchThresholdOpt: Option[Real],
        _lobeRangeOpt: Option[Real],
        _lobeRangePolesOpt: Option[String],
        _lobeBearingPolesOpt: Option[String])

    val kAnyKey = new Key(FieldModel.Glossary.kAnyKeyBase)

    val kNoneKey = new Key(FieldModel.Glossary.kNoneKeyBase)

    def DecodeConstructor (encoded: String): Constructor =
    {
        val constructor = Json.parse(encoded)

        val commonMeta = new CommonConstructorMetaDecoder(constructor, Cell.ErrorCodes.ProbeCollectorConstructorInvalid)

        val aliasedMeta = new AliasedConstructorMetaDecoder(constructor)

        val acoustic_aOpt: Option[Real] =
            (constructor \ FieldModel.Glossary.kPSAttrs \ FieldModel.Glossary.kPAttrsAcoustic_a).validate[String] match
            {
                case JsSuccess(value, _) => Some(value.toDouble)

                case JsError(errors) => None
            }

        val squelchThresholdOpt: Option[Real] =
            (constructor \ FieldModel.Glossary.kPSAttrs \ FieldModel.Glossary.kPAttrsSquelchThreshold).validate[String] match

            {
                case JsSuccess(value, _) => Some(value.toDouble)

                case JsError(errors) => None
            }

        val lobeRangeOpt: Option[Real] =
            (constructor \ FieldModel.Glossary.kPSAttrs \ FieldModel.Glossary.kPAttrsLobeRange).validate[String] match
            {
                case JsSuccess(value, _) => Some(value.toDouble)

                case JsError(errors) => None
            }

        val lobeRangePolesOpt: Option[String] =
            (constructor \ FieldModel.Glossary.kPSAttrs \ FieldModel.Glossary.kPAttrsLobeRangePoles).validate[String] match
            {
                case JsSuccess(value, _) => Some(value)

                case JsError(errors) => None
            }

        val lobeBearingPolesOpt: Option[String] =
            (constructor \ FieldModel.Glossary.kPSAttrs \ FieldModel.Glossary.kPAttrsLobeBearingPoles).validate[String] match
            {
                case JsSuccess(value, _) => Some(value)

                case JsError(errors) => None
            }

        new Constructor(
            commonMeta._tag,
            commonMeta._badgeOpt,
            commonMeta._nameOpt,
            commonMeta._descriptionOpt,
            aliasedMeta._aliasOpt,
            acoustic_aOpt,
            squelchThresholdOpt,
            lobeRangeOpt,
            lobeRangePolesOpt,
            lobeBearingPolesOpt)
    }

    def DecodeDestructor (encoded: String): Destructor =
    {
        val destructor = Json.parse(encoded)

        val commonMeta = new CommonDestructorMetaDecoder[ProbeCollector.Key](
            destructor, Cell.ErrorCodes.ProbeCollectorDestructorInvalid)

        new Destructor(commonMeta._key, commonMeta._scope)
    }

    def DecodeQuery (encoded: String): Query =
    {
        val query = Json.parse(encoded)

        val commonQuery = new CommonQueryDecoder[ProbeCollector.Key](query)

        new Query(commonQuery._keysOpt.get, commonQuery._sectionsOpt)
    }

    def DecodeUpdate (encoded: String): Update =
    {
        val update = Json.parse(encoded)

        val commonMeta = new CommonUpdateMetaDecoder[ProbeCollector.Key](
            update, Cell.ErrorCodes.ProbeCollectorUpdateInvalid)

        val acoustic_aOpt: Option[Real] =
            (update \ FieldModel.Glossary.kPSAttrs \ FieldModel.Glossary.kPAttrsAcoustic_a).validate[String] match
            {
                case JsSuccess(value, _) => Some(value.toDouble)

                case JsError(errors) => None
            }

        val squelchThresholdOpt: Option[Real] =
            (update \ FieldModel.Glossary.kPSAttrs \ FieldModel.Glossary.kPAttrsSquelchThreshold).validate[String] match

            {
                case JsSuccess(value, _) => Some(value.toDouble)

                case JsError(errors) => None
            }

        val lobeRangeOpt: Option[Real] =
            (update \ FieldModel.Glossary.kPSAttrs \ FieldModel.Glossary.kPAttrsLobeRange).validate[String] match
            {
                case JsSuccess(value, _) => Some(value.toDouble)

                case JsError(errors) => None
            }

        val lobeRangePolesOpt: Option[String] =
            (update \ FieldModel.Glossary.kPSAttrs \ FieldModel.Glossary.kPAttrsLobeRangePoles).validate[String] match
            {
                case JsSuccess(value, _) => Some(value)

                case JsError(errors) => None
            }

        val lobeBearingPolesOpt: Option[String] =
            (update \ FieldModel.Glossary.kPSAttrs \ FieldModel.Glossary.kPAttrsLobeBearingPoles).validate[String] match
            {
                case JsSuccess(value, _) => Some(value)

                case JsError(errors) => None
            }

        new Update(
            commonMeta._key,
            commonMeta._nameOpt,
            commonMeta._descriptionOpt,
            acoustic_aOpt,
            squelchThresholdOpt,
            lobeRangeOpt,
            lobeRangePolesOpt,
            lobeBearingPolesOpt)
    }
}

class ProbeCollector (
    meta: ProbeCollector.Meta,
    attrs: ProbeCollector.Attrs,
    refs: ProbeCollector.Refs,
    state: ProbeCollector.State = new ProbeCollector.State())
    (implicit
        protected val _fieldModel: FieldModel,
        protected val _trunkModel: TrunkModel)
    extends Collector[ProbeCollector.Key]
        with AliasedElement
{
    //
    // Meta:
    //
    protected
    val _meta = meta

    def GetAliasOpt = _meta._aliasOpt

    def SetAliasOpt (aliasOpt: Option[String]) = _meta._aliasOpt = aliasOpt

    //
    // Attrs:
    //
    protected
    val _attrs = attrs

    def GetAcoustic_a = _attrs._acoustic_a

    def GetLobeRangeOpt = _attrs._lobeRangeOpt

    def GetSquelchThresholdOpt = _attrs._squelchThresholdOpt

    def SetAcoustic_a (acoustic_a: Real) = _attrs._acoustic_a = acoustic_a

    def SetLobeBearingPolesOpt (lobeBearingPolesOpt: Option[String]) =
    {
        _attrs._lobeBearingPolesOpt = lobeBearingPolesOpt
        ImportBearingPoles()
    }

    def SetLobeRangePolesOpt (lobeRangePolesOpt: Option[String]) =
    {
        _attrs._lobeRangePolesOpt = lobeRangePolesOpt
        ImportRangePoles()
    }

    def SetLobeRangeOpt (lobeRangeOpt: Option[Real]) = _attrs._lobeRangeOpt = lobeRangeOpt

    def SetSquelchThresholdOpt (squelchThresholdOpt: Option[Real]) = _attrs._squelchThresholdOpt = squelchThresholdOpt

    //
    // Refs:
    //
    protected
    val _refs = refs

    def GetParentKey = _refs._parentKey

    //
    // State:
    //
    protected
    val _state = state

    def GetLobeBearingEnvelopeOpt: Option[Envelope.ContinuousEnvelope] = _state._bearingEnvelopeOpt

    def GetLobeRangeEnvelopeOpt: Option[Envelope.ContinuousEnvelope] = _state._distanceEnvelopeOpt

    //
    // Reports:
    //
    def Report (sectionsOpt: Option[String] = None): JsObject =
    {
        var report = Json.obj()

        val sections = new ReportSectionsParser(sectionsOpt)

        // Add meta section:
        if (sections.HasMetaPropertyset)
            report ++= Json.obj(FieldModel.Glossary.kPSMeta -> _meta.Report(sections))

        // Add attrs section:
        if (sections.HasAttrsPropertyset)
            report ++= Json.obj(FieldModel.Glossary.kPSAttrs -> _attrs.Report(sections))

        // Add refs section:
        if (sections.HasRefsPropertyset)
            report ++= Json.obj(FieldModel.Glossary.kPSRefs -> _refs.Report(sections))

        report
    }

    def ImportBearingPoles (): Unit =
    {
        val envelopeDef = _attrs._lobeBearingPolesOpt match
        {
            case Some(polesPacked) =>
                Json.obj(
                    FieldModel.Glossary.kEnvelopeCeiling -> 1.toString, // ceiling is always 1.0
                    FieldModel.Glossary.kEnvelopeFloor -> 0.toString, // floor is always 0
                    FieldModel.Glossary.kEnvelopePolesPackedDef -> polesPacked)

            case None => Envelope.kMaxGainEnvelopeDef
        }
        _state._bearingEnvelopeOpt = Some(Envelope.DecodeContinuousEnvelopeDef(envelopeDef))
    }

    def ImportRangePoles (): Unit =
    {
        val envelopeDef = _attrs._lobeRangePolesOpt match
        {
            case Some(polesPacked) =>
                Json.obj(
                    FieldModel.Glossary.kEnvelopeCeiling -> 1.toString, // ceiling is always 1.0
                    FieldModel.Glossary.kEnvelopeFloor -> 0.toString, // floor is always 0
                    FieldModel.Glossary.kEnvelopePolesPackedDef -> polesPacked)

            case None => Envelope.kMaxGainEnvelopeDef
        }
        _state._distanceEnvelopeOpt = Some(Envelope.DecodeContinuousEnvelopeDef(envelopeDef))
    }


    ImportBearingPoles()
    ImportRangePoles()
}
