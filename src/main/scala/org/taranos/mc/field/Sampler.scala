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
import org.taranos.mc.Common.Real
import play.api.libs.json.{JsError, JsSuccess, Json}


object Sampler
{
    case class Query (
        _fieldGeometryOpt: Option[String],
        _antipodeDistanceOpt: Option[Real],
        _collectorPositionOpt: Option[Body.Position],
        _collectorRotationOpt: Option[Body.Rotation],
        _acoustic_aOpt: Option[Real],
        _squelchThresholdOpt: Option[Real],
        _lobeRangeOpt: Option[Real],
        _lobeRangeEnvelopeOpt: Option[Envelope.ContinuousEnvelope],
        _lobeBearingEnvelopeOpt: Option[Envelope.ContinuousEnvelope],
        _sectionsOpt: Option[String])

    def DecodeQuery (encoded: String): Query =
    {
        val query = Json.parse(encoded)

        val fieldGeometryOpt: Option[String] =
            (query \ FieldModel.Glossary.kPAttrsFieldGeometry).validate[Vector[String]] match
            {
                case JsSuccess(value, _) => Some(value.head)

                case JsError(errors) => None
            }

        val antipodeDistanceOpt: Option[Real] =
            (query \ FieldModel.Glossary.kPAttrsAntipodeDistance).validate[Vector[String]] match
            {
                case JsSuccess(value, _) => Some(value.head.toDouble)

                case JsError(errors) => None
            }

        val collectorPositionOpt: Option[Body.Position] =
            (query \ FieldModel.Glossary.kPGeoCollectorPosition).validate[Vector[String]] match
            {
                case JsSuccess(value, _) =>
                    val decodedOpt = Body.DecodePosition(value.head)
                    decodedOpt match
                    {
                        case Some(pos2: Vector2) =>
                            Some(new Body.Position(pos2._x, pos2._y, 0f))

                        case Some(pos3: Vector3) =>
                            Some(new Body.Position(pos3._x, pos3._y, pos3._z))

                        case _ =>
                            throw new FieldException(Cell.ErrorCodes.PositionInvalid)
                    }

                case JsError(errors) => None
            }

        val collectorRotationOpt: Option[Body.Rotation] =
            (query \ FieldModel.Glossary.kPGeoCollectorRotation).validate[Vector[String]] match
            {
                case JsSuccess(value, _) =>
                    val decodedOpt = Body.DecodeRotation(value.head)
                    decodedOpt match
                    {
                        case Some(rot1: Vector1) =>
                            Some(new Body.Rotation(rot1._x, 0f, 0f, 0f))

                        case Some(rot4: Vector4) =>
                            Some(new Body.Rotation(rot4._w, rot4._x, rot4._y, rot4._z))

                        case _ =>
                            throw new FieldException(Cell.ErrorCodes.RotationInvalid)
                    }

                case JsError(errors) => None
            }

        val acoustic_aOpt: Option[Real] =
            (query \ FieldModel.Glossary.kPAttrsAcoustic_a).validate[Vector[String]] match
            {
                case JsSuccess(value, _) => Some(value.head.toDouble)

                case JsError(errors) => None
            }

        val squelchThresholdOpt: Option[Real] =
            (query \ FieldModel.Glossary.kPAttrsSquelchThreshold).validate[Vector[String]] match
            {
                case JsSuccess(value, _) => Some(value.head.toDouble)

                case JsError(errors) => None
            }

        val lobeRangeOpt: Option[Real] =
            (query \ FieldModel.Glossary.kPAttrsLobeRange).validate[Vector[String]] match
            {
                case JsSuccess(value, _) => Some(value.head.toDouble)

                case JsError(errors) => None
            }

        val lobeRangeEnvelopeOpt: Option[Envelope.ContinuousEnvelope] =
            (query \ FieldModel.Glossary.kPAttrsLobeRangePoles).validate[Vector[String]] match
            {
                case JsSuccess(value, _) =>
                    val polesPacked = value.head
                    val envelopeDef = Json.obj(
                        FieldModel.Glossary.kEnvelopeCeiling -> 1.toString, // ceiling is always 1.0
                        FieldModel.Glossary.kEnvelopeFloor -> 0.toString, // floor is always 0
                        FieldModel.Glossary.kEnvelopePolesPackedDef -> polesPacked)
                    val envelope = Envelope.DecodeContinuousEnvelopeDef(envelopeDef)
                    Some(envelope)

                case JsError(errors) => None
            }

        val lobeBearingEnvelopeOpt: Option[Envelope.ContinuousEnvelope] =
            (query \ FieldModel.Glossary.kPAttrsLobeBearingPoles).validate[Vector[String]] match
            {
                case JsSuccess(value, _) =>
                    val polesPacked = value.head
                    val envelopeDef = Json.obj(
                        FieldModel.Glossary.kEnvelopeCeiling -> 1.toString, // ceiling is always 1.0
                        FieldModel.Glossary.kEnvelopeFloor -> 0.toString, // floor is always 0
                        FieldModel.Glossary.kEnvelopePolesPackedDef -> polesPacked)
                    val envelope = Envelope.DecodeContinuousEnvelopeDef(envelopeDef)
                    Some(envelope)

                case JsError(errors) => None
            }

        val sectionsOpt: Option[String] =
            (query \ FieldModel.Glossary.kPQuerySections).validate[Vector[String]] match
            {
                case JsSuccess(value, _) => Some(value.head)

                case JsError(errors) => None
            }

        new Query(
            fieldGeometryOpt,
            antipodeDistanceOpt,
            collectorPositionOpt,
            collectorRotationOpt,
            acoustic_aOpt,
            squelchThresholdOpt,
            lobeRangeOpt,
            lobeRangeEnvelopeOpt,
            lobeBearingEnvelopeOpt,
            sectionsOpt)
    }
}
