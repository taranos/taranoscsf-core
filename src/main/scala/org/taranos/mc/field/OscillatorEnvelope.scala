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
import play.api.libs.json.{JsError, JsObject, JsSuccess, Json}


object OscillatorEnvelope
{
    class Key(uniqueKey: FieldElement.UniqueKey, symbol: Symbol = 'OscillatorEnvelope)
        extends FieldElement.Key(uniqueKey, symbol)

    case class Query (
        keys: Vector[OscillatorEnvelope.Key],
        sectionsOpt: Option[String] = None)
        extends FieldElement.Query[OscillatorEnvelope.Key](
            keys,
            sectionsOpt)

    case class Update (
        _key: Key,
        _envelopeDef: JsObject)

    val kAnyKey = new Key(FieldModel.Glossary.kAnyKeyBase)

    def DecodeQuery (encoded: String): Query =
    {
        val query = Json.parse(encoded)

        val keys: Vector[OscillatorEnvelope.Key] =
            (query \ FieldModel.Glossary.kPMetaKey).validate[Vector[String]] match
            {
                case JsSuccess(value, _) => value.map(new OscillatorEnvelope.Key(_))

                case JsError(errors) =>
                    val error = errors.head._2.head
                    error.message match
                    {
                        case "error.expected.jsarray" => Vector.empty[OscillatorEnvelope.Key]

                        case _ => throw FieldException(Cell.ErrorCodes.KeysInvalid)
                    }
            }

        val sectionsOpt: Option[String] =
            (query \ FieldModel.Glossary.kPQuerySections).validate[Vector[String]] match
            {
                case JsSuccess(value, _) => Some(value.head)

                case JsError(_) => None
            }

        Query(keys, sectionsOpt)
    }

    def DecodeUpdate (encoded: String): Update =
    {
        val update = Json.parse(encoded)

        val key: Key =
            (update \ FieldModel.Glossary.kPMetaKey).validate[String] match
            {
                case JsSuccess(value, _) => new Key(value)

                case JsError(_) => throw FieldException(
                    Cell.ErrorCodes.OscillatorPatchUpdateInvalid, "missing key property")
            }

        val envelopeDef: JsObject =
            (update \ FieldModel.Glossary.kEnvelopeDef).validate[JsObject] match
            {
                case JsSuccess(value, _) => value

                case JsError(_) => throw FieldException(
                    Cell.ErrorCodes.OscillatorPatchUpdateInvalid, "missing envelope def property")
            }

        Update(
            key,
            envelopeDef)
    }
}
