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
import org.taranos.mc.trunk.intraprocess.{Trunk, TrunkModel}


class FieldPlant
    (implicit
        protected val _fieldModel: FieldModel,
        protected val _trunkModel: TrunkModel)
    extends FieldElementPlant
{
    import scala.collection.mutable

    private
    var _fields = mutable.HashMap.empty[Field.Key, Field]

    def CreateField (
        constructor: Field.Constructor,
        trunk: Trunk): Field =
    {
        // 1: Create element:
        val field = new Field(
            new Field.Meta(
                FieldElement.MakeUniqueKey(constructor._tag),
                constructor._tag,
                constructor._badgeOpt,
                constructor._nameOpt,
                constructor._descriptionOpt),
            new Field.Attrs(
                constructor._geometryOpt.getOrElse(Field.Defaults.kGeometry),
                constructor._antipodeDistanceOpt.getOrElse(Field.Defaults.kAntipodeDistance),
                constructor._acoustic_cOpt.getOrElse(Field.Defaults.kAcoustic_c),
                constructor._acoustic_rOpt.getOrElse(Field.Defaults.kAcoustic_r)),
            new Field.Refs(trunk.GetKey))

        // 2: Add element to store:
        _fields += field.GetKey -> field

        // 3: Bind with/create children:
        // Create default emitter:
        val emitterConstructor = FieldEmitter.Constructor(
            _tag = constructor._tag + FieldModel.Glossary.kTagSeparator + FieldModel.Glossary.kEDefaultEmitter,
            _patchDefOpt = constructor._patchDefOpt,
            _modulatableKeyOpt = constructor._modulatorKeyOpt)
        _fieldModel.CreateFieldEmitters(
            field.GetKey,
            Vector(emitterConstructor))

        // 4: Bind with peers:
        // N/A

        // 5: Bind with parent:
        // N/A

        // 6: Activate element:
        // N/A

        // Return element:
        field
    }

    def DestroyField (destructor: Field.Destructor): Unit =
    {
        destructor._key match
        {
            case key: Field.Key =>
                _fields.get(key) match
                {
                    case Some(field) =>
                        // 1: Unbind with parent:
                        // N/A

                        // 2: Deactivate element:
                        // N/A

                        // 3: Unbind with peers:
                        // N/A

                        // 4: Unbind with/destroy children:
                        // We don't bother since it's expected plants of children will be destroyed soon.

                        // 5: Remove element from store:
                        _fields -= field.GetKey

                    case None => throw FieldException(Cell.ErrorCodes.FieldUnknown)
                }

            case _ => throw FieldException(Cell.ErrorCodes.FieldInvalid)
        }
    }

    def DestroyAllFields (scope: Symbol): Unit =
    {
        // Destroy each field of cell:
        _fields.foreach(fieldPair =>
        {
            val (fieldKey, _) = fieldPair
            val fieldDestructor = Field.Destructor(fieldKey, scope)
            DestroyField(fieldDestructor)
        })
    }

    def GetFieldOpt (
        key: Field.Key,
        isRequired: Boolean = true): Option[Field] =
    {
        // Lookup field :
        key match
        {
            case _: Field.Key =>
                val opt = _fields.get(key)
                if (isRequired && opt.isEmpty)
                    throw FieldException(Cell.ErrorCodes.FieldUnknown)
                opt

            case _ => throw FieldException(Cell.ErrorCodes.FieldKeyInvalid)
        }
    }

    def GetFieldKeys: Vector[Field.Key] =
    {
        // Return field keys vector:
        _fields.keys.toVector
    }

    def GetElementCount (fieldKey: Field.Key): Int =
        _fields.count(_._1 == fieldKey)
}
